#ifndef __OG_BASIC_AST_VISITOR_H__
#define __OG_BASIC_AST_VISITOR_H__

#include <string>
#include <memory>
#include <iostream>
#include <queue>
#include <cdk/compiler.h>
#include <cdk/symbol_table.h>
#include "targets/symbol.h"

/* do not edit -- include node forward declarations */
#define __NODE_DECLARATIONS_ONLY__
#include "ast/all.h"  // automatically generated
#undef __NODE_DECLARATIONS_ONLY__
/* do not edit -- end */

//!
//! Print nodes as XML elements to the output stream.
//!
class basic_ast_visitor {
protected:
  //! The owner compiler
  std::shared_ptr<cdk::compiler> _compiler;

private:

  std::queue<std::shared_ptr<og::symbol>> _symbols;

protected:
  basic_ast_visitor(std::shared_ptr<cdk::compiler> compiler) :
      _compiler(compiler) {
  }

  bool debug() {
    return _compiler->debug();
  }

  std::ostream &os() {
    return *_compiler->ostream();
  }

public:
  virtual ~basic_ast_visitor() {
  }

public:
  std::shared_ptr<og::symbol> pop_symbol() {
    if (_symbols.empty()) return nullptr;

    auto s = _symbols.front();
    _symbols.pop();
    return s;
  }

  void push_symbol(std::shared_ptr<og::symbol> symbol) {
    _symbols.push(symbol);
  }

public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#define __PURE_VIRTUAL_DECLARATIONS_ONLY__
#include "ast/visitor_decls.h"       // automatically generated
#undef __PURE_VIRTUAL_DECLARATIONS_ONLY__
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

};

#endif
