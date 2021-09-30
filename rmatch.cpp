#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <fmt/core.h>
#include <fmt/format.h>
#include <ios>
#include <iterator>
#include <list>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>
#include <xbyak.h>
#include <xbyak_util.h>

#ifdef XBYAK32
#error "this matcher is for only 64-bit mode"
#endif

using namespace std;
using fmt::format;
using fmt::print;

/*
  expr    ::= term+ ('|' expr)? ;
  term    ::= factor ('+' | '*' | '?')? ;
  factor  ::= '.' | char | escaped_char | char_set | '(' expr ')' ;
*/

enum Instruction {
  SPLIT,
  SPLIT_ONE,
  SINGLE,
  CHARSET,
  ANY,
  JUMP,
  ACCEPT,
  LABEL,
  STRING,
};

string_view name[] = {
    "SPLIT", "SPLIT_ONE", "SINGLE", "CHARSET", "ANY",
    "JUMP",  "ACCEPT",    "LABEL",  "STRING",
};

using match_prototype = bool (*)(const char *);

pair<char, char> extract_char_pair(uint32_t i) {
  return {(char)(i >> 8), (char)i};
}

uint32_t pack_char_pair(const pair<char, char> &p) {
  uint32_t c = p.first;
  return c << 8 | (uint32_t)p.second;
}

struct IR {
  list<uint32_t> instructions;
  vector<string> string_pool;
};

struct Codegen : Xbyak::CodeGenerator {
  Codegen() : Xbyak::CodeGenerator(4096, Xbyak::AutoGrow) {}
  void gen(const IR &ir) {
    vector<uint32_t> linear_inst(ir.instructions.cbegin(),
                                 ir.instructions.cend());
    Xbyak::util::StackFrame sf(
        this, 1, Xbyak::util::UseRCX | Xbyak::util::UseRDX, 0, false);
    const auto &a0 = sf.p[0];
    setDefaultJmpNEAR(true);
    L("reach_string_end");
    {
      push(rbp);
      mov(rbp, rsp);
      movzx(eax, byte[a0]);
      cmp(al, 0);
      jnz("run");
    }
    L("match_fail");
    { xor_(eax, eax); }
    L("match_return");
    {
      mov(rsp, rbp);
      pop(rbp);
      sf.close();
    }
    L("thread_fail");
    {
      cmp(rsp, rbp);
      je("match_fail");
      pop(a0);
      pop(r9);
      jmp(r9);
    }
    L("run");
    Xbyak::Label l;
    string s;
    for (size_t i = 0; i < linear_inst.size();) {
      switch (linear_inst[i]) {
      case SPLIT:
        s = format("L{}", linear_inst.at(i + 2));
        mov(rax, s.c_str());
        push(rax);
        push(a0);
        jmp(format("L{}", linear_inst.at(i + 1)));
        i += 3;
        break;
      case SPLIT_ONE:
        s = format("L{}", linear_inst.at(i + 1));
        mov(rax, s.c_str());
        push(rax);
        push(a0);
        i += 2;
        break;
      case SINGLE:
        cmp(byte[a0], linear_inst.at(i + 1));
        jne("thread_fail");
        inc(a0);
        i += 2;
        break;
      case CHARSET: {
        i += 1;
        int n = linear_inst.at(i);
        i += 1;
        xor_(eax, eax);
        movzx(edx, byte[a0]);
        cmp(dl, 0);
        jz("thread_fail");
        for (int j = 0; j < n; ++j, ++i) {
          auto [c1, c2] = extract_char_pair(linear_inst.at(i));
          if (c1 == c2) {
            cmp(dl, c1);
            sete(cl);
            or_(al, cl);
          } else {
            lea(ecx, ptr[rdx - c1]);
            cmp(cl, c2 - c1);
            setbe(cl);
            or_(al, cl);
          }
        }
        jz("thread_fail");
        inc(a0);
        break;
      }
      case ANY:
        cmp(byte[a0], 0);
        jz("thread_fail");
        inc(a0);
        i += 1;
        break;
      case JUMP:
        jmp(format("L{}", linear_inst.at(i + 1)));
        i += 2;
        break;
      case ACCEPT:
        mov(eax, 1);
        jmp("match_return");
        i += 1;
        break;
      case LABEL:
        L(format("L{}", linear_inst.at(i + 1)));
        i += 2;
        break;
      case STRING: {
        const auto &s = ir.string_pool[linear_inst.at(i + 1)];
        auto p = s.data();
        auto len = s.length();
        while (len > 0) {
          if (len >= 8) {
            auto i = *reinterpret_cast<const uint64_t *>(p);
            mov(rax, i);
            cmp(rax, qword[a0]);
            jne("thread_fail");
            add(a0, 8);
            len -= 8;
            p += 8;
          } else if (len >= 4) {
            auto i = *reinterpret_cast<const uint32_t *>(p);
            cmp(dword[a0], i);
            jne("thread_fail");
            add(a0, 4);
            len -= 4;
            p += 4;
          } else if (len >= 2) {
            auto i = *reinterpret_cast<const uint16_t *>(p);
            cmp(word[a0], i);
            jne("thread_fail");
            add(a0, 2);
            len -= 2;
            p += 2;
          } else {
            cmp(byte[a0], *p);
            jne("thread_fail");
            inc(a0);
            len--;
            p++;
          }
        }
        i += 2;
        break;
      }
      }
    }
  }
};

struct Optimizer {
  IR ir;
  Optimizer(IR &ir) : ir(ir) {}
  void optimize() {
    split_jump_fusion();
    single_fusion();
  }

  void dump() {
    auto &instructions = ir.instructions;
    list<uint32_t>::iterator l1, l2;
    print("--------------------------\n");
    for (auto iter = instructions.begin(); iter != instructions.end();) {
      switch (*iter) {
      case SPLIT:
        l1 = ++iter;
        l2 = ++iter;
        print("  {} L{} L{}\n", name[SPLIT], *l1, *l2);
        ++iter;
        break;
      case SPLIT_ONE:
        l2 = ++iter;
        print("  {} L{}\n", name[SPLIT_ONE], *l2);
        ++iter;
        break;
      case LABEL:
        l1 = ++iter;
        print("L{}:\n", *l1);
        ++iter;
        break;
      case JUMP:
        l2 = ++iter;
        print("  {} L{}\n", name[JUMP], *l2);
        ++iter;
        break;
      case SINGLE:
        l2 = ++iter;
        print("  {} {}\n", name[SINGLE], (char)*l2);
        ++iter;
        break;
      case CHARSET: {
        int n = *++iter;
        ++iter;
        string s;
        for (int i = 0; i < n; ++i, ++iter) {
          auto [c1, c2] = extract_char_pair(*iter);
          if (c1 == c2)
            s.push_back(c1);
          else {
            s.push_back(c1);
            s.push_back('-');
            s.push_back(c2);
          }
        }
        print("  {} {}\n", name[CHARSET], s);
        break;
      }
      case ANY:
        print("  {}\n", name[ANY]);
        ++iter;
        break;
      case ACCEPT:
        print("  {}\n", name[ACCEPT]);
        ++iter;
        break;
      case STRING: {
        ++iter;
        auto &s = ir.string_pool[*iter++];
        print("  {} {}\n", name[STRING], s);
        break;
      }
      default:
        throw runtime_error("unexpected instruction");
      }
    }
  }

  void single_fusion() {
    auto &instructions = ir.instructions;
    for (auto iter = instructions.begin(); iter != instructions.end();) {
      switch (*iter) {
      case SPLIT:
        advance(iter, 3);
        break;
      case SPLIT_ONE:
      case LABEL:
      case JUMP:
      case STRING:
        advance(iter, 2);
        break;
      case SINGLE: {
        auto single_begin = iter++;
        auto ch = *iter++;
        string s{(char)ch};
        for (; iter != instructions.end() && *iter == SINGLE;) {
          s.push_back(*++iter);
          ++iter;
        }
        if (s.length() == 1)
          break;
        else {
          auto next_single = next(single_begin, 2);
          instructions.erase(next_single, iter);
          *single_begin++ = STRING;
          *single_begin = ir.string_pool.size();
          ir.string_pool.emplace_back(s);
        }
        break;
      }
      case CHARSET: {
        ++iter;
        auto n = *iter;
        for (int i = 0; i < n + 1; ++i)
          ++iter;
        break;
      }
      case ANY:
      case ACCEPT:
        advance(iter, 1);
        break;
      default:
        throw runtime_error("unexpected instruction");
      }
    }
  }

  void split_jump_fusion() {
    auto &instructions = ir.instructions;
    for (auto iter = instructions.begin(); iter != instructions.end();) {
      switch (*iter) {
      case SPLIT: {
        auto split = iter; // SPLIT
        auto l1 = ++iter;  // L1
        auto l2 = ++iter;  // L2
        ++iter;            // LABEL
        if (iter != instructions.end() && *iter == LABEL) {
          auto label = iter;
          auto lt = ++iter;
          if (*lt == *l1) { // need fusion
            instructions.erase(l1);
            *split = SPLIT_ONE;
          }
          ++iter;
        } else {
          advance(iter, 2);
        }
        break;
      }
      case SPLIT_ONE:
      case LABEL:
      case JUMP:
      case SINGLE:
      case STRING:
        advance(iter, 2);
        break;
      case CHARSET: {
        ++iter;
        auto n = *iter;
        advance(iter, n + 1);
        break;
      }
      case ANY:
      case ACCEPT:
        ++iter;
        break;
      default:
        throw runtime_error("unexpected instruction");
      }
    }
  }
};

struct Parser {
  string re;
  string::const_iterator iter;
  uint32_t label_id;
  vector<pair<char, char>> char_range;
  vector<pair<char, char>> char_range_res;

  Parser(const string &re) : re(re), iter(this->re.cbegin()), label_id() {}

  bool not_end() { return iter != re.cend(); }

  void next_iter() { iter++; }

  char escaped(string::const_iterator iter, string::const_iterator end) {
    if (*iter == '\\' && ++iter != end) {
      return *iter;
    }
    if (iter == end)
      throw runtime_error("escaped sequence followed by EOF");
    return *iter;
  }

  IR parse_all() { return {parse(0)}; }

  list<uint32_t> parse(size_t l) {
    switch (l) {
    case 0: {
      list<uint32_t> inst;
      auto r = parse(1);
      inst.insert(inst.begin(), r.cbegin(), r.cend());
      inst.push_back(ACCEPT);
      return inst;
    }
    case 1: {

      list<uint32_t> inst;
      while (not_end() && *iter != '|' && *iter != ')') {
        auto r = parse(2);
        inst.insert(inst.end(), r.cbegin(), r.cend());
      }
      if (not_end() && *iter == '|') {
        list<uint32_t> new_inst{SPLIT, label_id, label_id + 1, LABEL, label_id};
        inst.insert(inst.begin(), new_inst.cbegin(), new_inst.cend());
        inst.push_back(JUMP);
        inst.push_back(label_id + 2);
        inst.push_back(LABEL);
        inst.push_back(label_id + 1);
        auto l3 = label_id + 2;
        label_id += 3;
        next_iter();
        auto r = parse(1);
        inst.insert(inst.end(), r.cbegin(), r.cend());
        inst.push_back(LABEL);
        inst.push_back(l3);
      }
      return inst;
    }
    case 2: {
      list<uint32_t> inst;
      if (not_end() && *iter != '|' && *iter != ')' && *iter != '+' &&
          *iter != '*' && *iter != '?') {
        auto r = parse(3);
        inst.insert(inst.end(), r.cbegin(), r.cend());
      }
      if (not_end())
        switch (*iter) {
        case '+': {
          inst.push_front(label_id);
          inst.push_front(LABEL);

          list<uint32_t> new_inst{SPLIT, label_id, label_id + 1, LABEL,
                                  label_id + 1};
          inst.insert(inst.end(), new_inst.cbegin(), new_inst.cend());
          label_id += 2;
          next_iter();
          return inst;
        }

        case '*': {
          list<uint32_t> new_inst1{SPLIT, label_id, label_id + 1, LABEL,
                                   label_id};
          list<uint32_t> new_inst2{SPLIT, label_id, label_id + 1, LABEL,
                                   label_id + 1};
          inst.insert(inst.begin(), new_inst1.cbegin(), new_inst1.cend());
          inst.insert(inst.end(), new_inst2.cbegin(), new_inst2.cend());
          label_id += 2;
          next_iter();
          return inst;
        }
        case '?': {
          list<uint32_t> new_inst{SPLIT, label_id, label_id + 1, LABEL,
                                  label_id};
          inst.insert(inst.begin(), new_inst.cbegin(), new_inst.cend());
          inst.push_back(LABEL);
          inst.push_back(label_id + 1);
          label_id += 2;
          next_iter();
          return inst;
        }
        }
      return inst;
    }
    case 3: {
      list<uint32_t> inst;
      if (!not_end())
        return inst;
      switch (*iter) {
      case '.':
        inst.push_back(ANY);
        next_iter();
        return inst;
      case '\\':
        inst.push_back(SINGLE);
        inst.push_back(escaped(iter, re.cend()));
        next_iter();
        return inst;
      case '(': {
        next_iter();
        auto r = parse(1);
        inst.insert(inst.begin(), r.cbegin(), r.cend());
        if (!not_end() || *iter != ')')
          throw runtime_error("invalid tokens after '('");
        next_iter();
        return inst;
      }
      case '[': {
        next_iter();
        if (!not_end() || *iter == ']')
          throw runtime_error("invalid tokens after '['");
        char_range.clear();
        char_range_res.clear();
        while (*iter != ']') {
          char c1 = escaped(iter, re.cend());
          char c2 = c1;
          next_iter();
          if (iter != re.cend() && iter + 1 != re.cend() && *iter == '-' &&
              *(iter + 1) != ']') {
            next_iter();
            c2 = escaped(iter, re.cend());
            if (c2 <= c1)
              throw runtime_error(
                  "the lower bound is larger than the upper bound");
            next_iter();
          }
          char_range.emplace_back(c1, c2);
        }
        sort(char_range.begin(), char_range.end(),
             [](pair<char, char> &p1, pair<char, char> &p2) {
               return p1.first < p2.first;
             });

        char_range_res.push_back(char_range.at(0));
        for (int i = 1; i < char_range.size(); ++i) {
          if (char_range_res.back().second < char_range[i].first)
            char_range_res.push_back(char_range[i]);
          else
            char_range_res.back().second =
                max(char_range_res.back().second, char_range[i].second);
        }
        inst.push_back(CHARSET);
        inst.push_back((uint32_t)char_range_res.size());
        for (auto &&p : char_range_res)
          inst.push_back(pack_char_pair(p));
        next_iter();
        return inst;
      }
      default:
        inst.push_back(SINGLE);
        inst.push_back(*iter);
        next_iter();
        return inst;
      }
    }
    }
    return {};
  }
};

int main() {
  string re("abcdefg");
  Parser parser(re);
  auto ir = parser.parse_all();
  Optimizer opt(ir);
  opt.optimize();
  Codegen codegen;
  codegen.gen(opt.ir);
  codegen.readyRE();
  auto f = codegen.getCode<match_prototype>();
  fwrite(codegen.getCode(), 1, codegen.getSize(), stdout);
  return f("abcdefg");
}