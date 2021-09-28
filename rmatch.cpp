#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <fmt/core.h>
#include <fmt/format.h>
#include <ios>
#include <list>
#include <memory>
#include <stdexcept>
#include <stdint.h>
#include <string>
#include <string_view>
#include <xbyak.h>
#include <xbyak_util.h>

#ifdef XBYAK32
#error "this matcher is for only 64-bit mode"
#endif

using namespace std;
using fmt::format;
using fmt::print;

/*
  regex   ::= '^'? expr '$'? ;
  expr    ::= term+ ('|' expr)? ;
  term    ::= factor ('+' | '*' | '?')? ;
  factor  ::= '.' | char | escaped_char | '(' expr ')' ;
*/

enum Instruction {
  SPLIT,
  SPLIT_ONE,
  SINGLE,
  ANY,
  JUMP,
  ACCEPT,
  LABEL,
};

string_view name[] = {
    "SPLIT", "SPLIT_ONE", "SINGLE", "ANY", "JUMP", "ACCEPT", "LABEL",
};

using match_prototype = bool (*)(const char *);

struct Compiler : Xbyak::CodeGenerator {
  Compiler() : Xbyak::CodeGenerator(4096, Xbyak::AutoGrow) {}
  void compile(const list<uint32_t> &instructions) {
    vector<uint32_t> linear_inst(instructions.cbegin(), instructions.cend());
    Xbyak::util::StackFrame sf(this, 1, 0, 0, false);
    const auto &a0 = sf.p[0];
    setDefaultJmpNEAR(true);
    L("reach_string_end");
    {
      push(rbp);
      mov(rbp, rsp);
      movzx(eax, byte[a0]);
      jmp("run");
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
        sub(rsp, 16);
        mov(ptr[rsp], a0);
        mov(ptr[rsp + 8], rax);
        jmp(format("L{}", linear_inst.at(i + 1)));
        i += 3;
        break;
      case SPLIT_ONE:
        s = format("L{}", linear_inst.at(i + 1));
        mov(rax, s.c_str());
        sub(rsp, 16);
        mov(ptr[rsp], a0);
        mov(ptr[rsp + 8], rax);
        i += 2;
        break;
      case SINGLE:
        cmp(byte[a0], linear_inst.at(i + 1));
        jne("thread_fail");
        inc(a0);
        i += 2;
        break;
      case ANY:
        movzx(eax, byte[a0]);
        jnz("thread_fail");
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
      }
    }
  }
};

struct Optimizer {
  list<uint32_t> &instructions;
  Optimizer(list<uint32_t> &instructions) : instructions(instructions) {}
  void optimize() { split_jump_fusion(); }

  template <int n> void skip(list<uint32_t>::iterator &iter) {
    if constexpr (n <= 0)
      return;
    else {
      iter++;
      skip<n - 1>(iter);
    }
  }

  void dump() {
    list<uint32_t>::iterator l1, l2;
    print("-------------\n");
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
      case ANY:
        print("  {}\n", name[ANY]);
        ++iter;
        break;
      case ACCEPT:
        print("  {}\n", name[ACCEPT]);
        ++iter;
        break;
      }
    }
  }

  void split_jump_fusion() {
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
          skip<2>(iter);
        }
        break;
      }
      case SPLIT_ONE:
      case LABEL:
      case JUMP:
      case SINGLE:
        skip<2>(iter);
        break;
      case ANY:
      case ACCEPT:
        skip<1>(iter);
        break;
      }
    }
  }
};

struct Parser {
  string re;
  string::const_iterator iter;
  uint32_t label_id;

  Parser(const string &re) : re(re), iter(this->re.cbegin()), label_id() {}

  bool not_end() { return iter != re.cend(); }

  void next_iter() { iter++; }

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
          inst.push_front(LABEL);
          inst.push_front(label_id);

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
        next_iter();
        if (iter == re.cend())
          throw runtime_error(
              "escaped character reaches the end of expression");
        inst.push_back(SINGLE);
        inst.push_back(*iter);
        next_iter();
        return inst;
      case '(': {
        next_iter();
        auto r = parse(1);
        inst.insert(inst.begin(), r.cbegin(), r.cend());
        if (!not_end() || *iter != ')')
          throw runtime_error("");
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
  string re("(a|b)*d?c(0|1|2|3|4|5|6|7|8|9)12345678");
  Parser matcher(re);
  auto inst = matcher.parse(0);
  Optimizer opt(inst);
  opt.optimize();
  Compiler compiler;
  compiler.compile(opt.instructions);
  compiler.readyRE();
  auto f = compiler.getCode<match_prototype>();
  fwrite(compiler.getCode(), 1, compiler.getSize(), stdout);
}