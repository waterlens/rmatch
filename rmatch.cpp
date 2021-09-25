#include <cstddef>
#include <cstdint>
#include <fmt/core.h>
#include <fmt/format.h>
#include <list>
#include <memory>
#include <stdexcept>
#include <stdint.h>
#include <string>
#include <string_view>
#include <xbyak.h>

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
  MATCH,
  JUMP,
  BEGIN,
  END,
  ACCEPT,
  LABEL,
  ANY,
  SINGLE,
};

struct Matcher : Xbyak::CodeGenerator {
  string re;
  string::const_iterator iter;
  uint32_t label_id;
  list<uint32_t> instructions;

  Matcher(const string &re)
      : Xbyak::CodeGenerator(4096, Xbyak::AutoGrow), re(re),
        iter(this->re.cbegin()), label_id() {}

  bool not_end() { return iter != re.cend(); }

  void next_iter() { iter++; }

  list<uint32_t> parse(size_t l) {
    switch (l) {
    case 0: {
      list<uint32_t> inst;
      if (not_end() && *iter == '^') {
        inst.push_back(BEGIN);
        next_iter();
      }
      auto r = parse(1);
      inst.insert(inst.end(), r.cbegin(), r.cend());
      if (not_end() && *iter == '$') {
        inst.push_back(END);
        next_iter();
      }
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
  string re("(a|b)*abb+c?");
  Matcher match(re);
  for (auto &&i : match.parse(0)) {
    print("{}\n", i);
  }
}