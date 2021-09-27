#include <cstddef>
#include <cstdint>
#include <cstdio>
#include <fmt/core.h>
#include <fmt/format.h>
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

/*
  entry:
    ; allocate stack frame
    mov   a0, st.p[0]
  reach_string_end:
    movzx eax, byte ptr [a0]
    jnz   run
  match_fail:
    xor   eax, eax
  match_return:
    ; clear stack and return
  backtrack:
    cmp   rsp, rbp
    je    match_fail
    pop   a0
    pop   r9
    jmp   r9

*/

enum Instruction {
  /*
    push  L2
    push  a0
    jmp   L1
   */
  SPLIT,
  /*
    cmp   byte ptr [a0], `char`
    jne   thread_fail
    inc   a0
   */
  SINGLE,
  /*
    movzx eax, byte ptr [a0]
    jz    thread_fail
    inc   a0
   */
  ANY,
  /*
    jmp L1
   */
  JUMP,
  /*
    mov   eax, 1
    jmp   match_return
   */
  ACCEPT,
  LABEL,
};
using match_prototype = bool (*)(const char *);

struct Compiler : Xbyak::CodeGenerator {
  Compiler() : Xbyak::CodeGenerator(4096, Xbyak::AutoGrow) {}
  void compile(const list<uint32_t> &instructions) {
    vector<uint32_t> linear_inst(instructions.cbegin(), instructions.cend());
    Xbyak::util::StackFrame sf(this, 1, 0, 0, false);
    const auto &a0 = sf.p[0];
    L("reach_string_end");
    {
      movzx(eax, byte[a0]);
      push(rbp);
      mov(rbp, rsp);
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
        s = format("L{}", linear_inst.at(i + 1));
        mov(rax, s.c_str());
        sub(rsp, 16);
        mov(ptr[rsp], a0);
        mov(ptr[rsp + 8], rax);
        jmp(format("L{}", linear_inst.at(i + 2)));
        i += 3;
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

struct Matcher {
  string re;
  string::const_iterator iter;
  uint32_t label_id;

  Matcher(const string &re) : re(re), iter(this->re.cbegin()), label_id() {}

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
      /*
          split l1 l2
        l1:
          ...
          jump l3
        l2:
          ...
        l3:
      */

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
          /*
            l1:
              ...
              split l1 l2
            l2:
          */
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
          /*
              split l1 l2
            l1:
              ...
              split l1 l2
            l2:
          */
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
          /*
              split l1 l2
            l1:
              ...
            l2:
          */
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
  string re("(a|b)");
  Matcher matcher(re);
  Compiler compiler;
  compiler.compile(matcher.parse(0));
  compiler.readyRE();
  auto f = compiler.getCode<match_prototype>();
  fwrite(compiler.getCode(), 1, compiler.getSize(), stdout);
  return f("a");
}