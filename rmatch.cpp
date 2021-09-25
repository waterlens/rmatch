#include <cstddef>
#include <fmt/core.h>
#include <fmt/format.h>
#include <list>
#include <memory>
#include <stdexcept>
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

struct matcher : Xbyak::CodeGenerator {
  string re;
  string::const_iterator iter;
  size_t label_id;
  matcher(const string &re)
      : Xbyak::CodeGenerator(4096, Xbyak::AutoGrow), re(re),
        iter(this->re.cbegin()), label_id() {}

  bool not_end() { return iter != re.cend(); }

  void next_iter() { iter++; }

  string parse(size_t l) {
    switch (l) {
    case 0: {
      string s;
      if (not_end() && *iter == '^') {
        s += "    match ^\n";
        next_iter();
      }
      s += parse(1);
      if (not_end() && *iter == '$') {
        s += "    match $\n";
        next_iter();
      }
      return s;
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
      string s;
      while (not_end() && *iter != '|' && *iter != ')')
        s += parse(2);
      if (not_end() && *iter == '|') {
        auto l3 = label_id + 2;
        s = format("    split L{} L{}\nL{}:\n", label_id, label_id + 1,
                   label_id) +
            s + format("    jump L{}\nL{}:\n", label_id + 2, label_id + 1);
        label_id += 3;
        next_iter();
        s += parse(1);
        s += format("L{}:\n", l3);
      }
      return s;
    }
    case 2: {
      string s;
      if (not_end() && *iter != '|' && *iter != ')' && *iter != '+' &&
          *iter != '*' && *iter != '?') {
        s += parse(3);
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
          s = format("L{}:\n", label_id) + s +
              format("    split L{} L{}\nL{}:\n", label_id, label_id + 1,
                     label_id + 1);
          label_id += 2;
          next_iter();
          return s;
        }

        case '*':
          /*
              split l1 l2
            l1:
              ...
              split l1 l2
            l2:
          */
          s = format("    split L{} L{}\nL{}:\n", label_id, label_id + 1,
                     label_id) +
              s +
              format("    split L{} L{}\nL{}:\n", label_id, label_id + 1,
                     label_id + 1);
          label_id += 2;
          next_iter();
          return s;
        case '?': {
          /*
              split l1 l2
            l1:
              ...
            l2:
          */
          s = format("    split L{} L{}\nL{}:\n", label_id, label_id + 1,
                     label_id) +
              s + format("L{}:\n", label_id + 1);
          label_id += 2;
          next_iter();
          return s;
        }
        }
      return s;
    }
    case 3: {
      string s;
      if (!not_end())
        return s;
      switch (*iter) {
      case '.':
        s += "   any";
        next_iter();
        return s;
      case '\\':
        next_iter();
        if (iter == re.cend())
          throw runtime_error(
              "escaped character reaches the end of expression");
        s += format("    match '{}'\n", *iter);
        next_iter();
        return s;
      case '(':
        next_iter();
        s += parse(1);
        if (!not_end() || *iter != ')')
          throw runtime_error("");
        next_iter();
        return s;
      default:
        s += format("    match '{}'\n", *iter);
        next_iter();
        return s;
      }
    }
    }
    return "";
  }
};

int main() {
  string re("(a|b)*abb+c?");
  matcher match(re);
  print("{}", match.parse(0));
}