#ifndef __OG_TARGETS_TYPE_CHECKER_H__
#define __OG_TARGETS_TYPE_CHECKER_H__

#include "targets/basic_ast_visitor.h"

namespace og {

  /**
   * Print nodes as XML elements to the output stream.
   */
  class type_checker: public basic_ast_visitor {
    cdk::symbol_table<og::symbol> &_symtab;

    basic_ast_visitor *_parent;

    int &_offset;

    std::shared_ptr<og::symbol> &_function;

    bool &_in_args;

  public:
    type_checker(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<og::symbol> &symtab,
    basic_ast_visitor *parent, std::shared_ptr<og::symbol> &function, int &offset, bool &in_args) :
        basic_ast_visitor(compiler), _symtab(symtab), _parent(parent), _offset(offset), _function(function), _in_args(in_args) {
    }

  public:
    ~type_checker() {
      os().flush();
    }

  protected:
    void processUnaryExpression(cdk::unary_operation_node *const node, int lvl);
    void processIntegerBinaryExpression(cdk::binary_operation_node *const node, int lvl);
    void processMultiplicationDivision(cdk::binary_operation_node *const node, int lvl);
    void processComparison(cdk::binary_operation_node *const node, int lvl);
    void processEquality(cdk::binary_operation_node *const node, int lvl);
    template<typename T>
    void process_literal(cdk::literal_node<T> *const node, int lvl) {
    }
    void binaryOperationTypeError(cdk::binary_operation_node *const node);

  protected:
    void do_ScalarLogicalExpression(cdk::binary_operation_node * const node, int lvl);
    void do_BooleanLogicalExpression(cdk::binary_operation_node * const node, int lvl);
    void do_GeneralLogicalExpression(cdk::binary_operation_node * const node, int lvl);

  private:
    bool deep_type_check(std::shared_ptr<cdk::basic_type> l, std::shared_ptr<cdk::basic_type> r);
    bool assignment_compatible(std::shared_ptr<cdk::basic_type> l, std::shared_ptr<cdk::basic_type> r);
    void check_variable_declaration(og::variable_declaration_node *const node, std::shared_ptr<og::symbol> symbol, std::shared_ptr<cdk::basic_type> type);
    void check_variable_definition(og::variable_declaration_node *const node, std::shared_ptr<og::symbol> symbol);
    void check_function_declaration(og::function_declaration_node *const node, std::shared_ptr<og::symbol> symbol);
    void hint_type(std::shared_ptr<cdk::basic_type> lt, cdk::typed_node *const r);

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include "ast/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end

  };

  bool is_typed(std::shared_ptr<cdk::basic_type> type, cdk::typename_type name);
  std::shared_ptr<cdk::basic_type> referenced(std::shared_ptr<cdk::basic_type> type);
  bool is_void_pointer(std::shared_ptr<cdk::basic_type> type);

} // og

//---------------------------------------------------------------------------
//     HELPER MACRO FOR TYPE CHECKING
//---------------------------------------------------------------------------

#define CHECK_TYPES(compiler, symtab, node, function, offset, args) { \
  try { \
    og::type_checker checker(compiler, symtab, this, function, offset, args); \
    (node)->accept(&checker, 0); \
  } \
  catch (const std::string &problem) { \
    std::cerr << (node)->lineno() << ": " << problem << std::endl; \
    return; \
  } \
}

#define ASSERT_SAFE_EXPRESSIONS CHECK_TYPES(_compiler, _symtab, node, _function, _offset, _in_args)

#endif
