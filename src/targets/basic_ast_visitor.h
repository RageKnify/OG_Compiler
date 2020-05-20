#ifndef __OG_BASIC_AST_VISITOR_H__
#define __OG_BASIC_AST_VISITOR_H__

#include <string>
#include <memory>
#include <iostream>
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

  bool _in_function;

private:

  // last symbol inserted in symbol table
  std::vector<std::shared_ptr<og::symbol>> _new_symbols;

protected:
  basic_ast_visitor(std::shared_ptr<cdk::compiler> compiler) :
      _compiler(compiler), _in_function(false) {
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
  std::shared_ptr<og::symbol> new_symbol() {
    return _new_symbols.empty() ? nullptr : _new_symbols[0];
  }
  std::vector<std::shared_ptr<og::symbol>>& new_symbols() {
    return _new_symbols;
  }

  void set_new_symbol(std::shared_ptr<og::symbol> symbol) {
    _new_symbols.push_back(symbol);
  }

  void reset_new_symbol() {
    _new_symbols.clear();
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
