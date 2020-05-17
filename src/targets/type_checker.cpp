#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "ast/all.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#include "og_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

bool og::type_checker::deep_type_check(std::shared_ptr<cdk::basic_type> l, std::shared_ptr<cdk::basic_type> r) {
  while (l->name() == cdk::TYPE_POINTER && r->name() == cdk::TYPE_POINTER)
  {
    l = cdk::reference_type_cast(l)->referenced();
    r = cdk::reference_type_cast(r)->referenced();
  }

  return l == r;
}

//---------------------------------------------------------------------------

void og::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); ++i) {
	  node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void og::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void og::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void og::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::make_primitive_type(8, cdk::TYPE_DOUBLE));
}
void og::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  // EMPTY
}

void og::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}

void og::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  do_BooleanLogicalExpression(node, lvl);
}

//---------------------------------------------------------------------------

void og::type_checker::do_BooleanLogicalExpression(cdk::binary_operation_node * const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() != cdk::TYPE_INT) throw std::string("integer expression expected in binary expression");

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() != cdk::TYPE_INT) throw std::string("integer expression expected in binary expression");

  node->type(node->left()->type());
}

//---------------------------------------------------------------------------

void og::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
}

void og::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::make_primitive_type(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void og::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  node->type(node->argument()->type());
}

void og::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  ASSERT_UNSPEC;
  processUnaryExpression(node, lvl);
  if (! (node->argument()->is_typed(cdk::TYPE_INT) ||
        node->argument()->is_typed(cdk::TYPE_DOUBLE)) ) {
    throw std::string("integer or real expression expected by symmetry operator");
  }
}

void og::type_checker::do_identity_node(og::identity_node *const node, int lvl) {
  ASSERT_UNSPEC;
  processUnaryExpression(node, lvl);
  if (! (node->argument()->is_typed(cdk::TYPE_INT) ||
        node->argument()->is_typed(cdk::TYPE_DOUBLE)) ) {
    throw std::string("integer or real expression expected by identity operator");
  }
}

//---------------------------------------------------------------------------

void og::type_checker::binaryOperationTypeError(cdk::binary_operation_node *const node) {
  std::ostringstream os;
  os << "wrong operand types for binary expression: ";
  os << cdk::to_string(node->left()->type()) << " and ";
  os << cdk::to_string(node->right()->type());
  throw os.str();
}

void og::type_checker::processIntegerBinaryExpression(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT))) {
    binaryOperationTypeError(node);
  }

  node->type(node->left()->type());
}

void og::type_checker::processMultiplicationDivision(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(node->left()->is_typed(cdk::TYPE_INT)  || node->left()->is_typed(cdk::TYPE_DOUBLE))  ||
      !(node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
        binaryOperationTypeError(node);
      }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->left()->type());
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->right()->type());
  } else {
    // TYPE_INT
    node->type(node->left()->type());
  }
}

void og::type_checker::processComparison(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(node->left()->is_typed(cdk::TYPE_INT)  || node->left()->is_typed(cdk::TYPE_DOUBLE)) ||
      !(node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE))) {
        binaryOperationTypeError(node);
    }

  if (node->left()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->right()->type());
  } else {
    node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
  }
}

void og::type_checker::processEquality(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if ((node->left()->is_typed(cdk::TYPE_DOUBLE)  && node->right()->is_typed(cdk::TYPE_POINTER)) ||
      (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
        binaryOperationTypeError(node);
      }

  if (node->left()->is_typed(cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (node->right()->is_typed(cdk::TYPE_INT)) {
    node->type(node->right()->type());
  } else {
    node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
  }
}

void og::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(node->left()->is_typed(cdk::TYPE_INT)  || node->left()->is_typed(cdk::TYPE_DOUBLE)  || node->left()->is_typed(cdk::TYPE_POINTER))  ||
      !(node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_POINTER)) ||
      (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_DOUBLE)) ||
      (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_POINTER)) ||
      (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER))) {
        binaryOperationTypeError(node);
  }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->left()->type());
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->right()->type());
  } else if (node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->left()->type());
  } else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else {
    // TYPE_INT
    node->type(node->left()->type());
  }
}
void og::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(node->left()->is_typed(cdk::TYPE_INT)  || node->left()->is_typed(cdk::TYPE_DOUBLE)  || node->left()->is_typed(cdk::TYPE_POINTER))  ||
      !(node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_POINTER)) ||
      (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_DOUBLE)) ||
      (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_POINTER))) {
        binaryOperationTypeError(node);
  }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->left()->type());
  } else if (node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    node->type(node->right()->type());
  } else if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    if (!deep_type_check(node->left()->type(), node->right()->type()))
      binaryOperationTypeError(node);

    node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
  } else if (node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->left()->type());
  } else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
    node->type(node->right()->type());
  } else {
    // TYPE_INT
    node->type(node->left()->type());
  }
}
void og::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processMultiplicationDivision(node, lvl);
}
void og::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processMultiplicationDivision(node, lvl);
}
void og::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT))) {
    binaryOperationTypeError(node);
  }

  node->type(node->left()->type());
}
void og::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processComparison(node, lvl);
}
void og::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processComparison(node, lvl);
}
void og::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processComparison(node, lvl);
}
void og::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processComparison(node, lvl);
}
void og::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processEquality(node, lvl);
}
void og::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processEquality(node, lvl);
}

//---------------------------------------------------------------------------

void og::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<og::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void og::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void og::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  try {
    node->lvalue()->accept(this, lvl);
  } catch (const std::string &id) {
    auto symbol = std::make_shared<og::symbol>(cdk::make_primitive_type(4, cdk::TYPE_INT), id);
    _symtab.insert(id, symbol);
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
    node->lvalue()->accept(this, lvl);  //DAVID: bah!
  }

  if (!node->lvalue()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of assignment expression");

  node->rvalue()->accept(this, lvl + 2);
  if (!node->rvalue()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of assignment expression");

  // in Simple, expressions are always int
  node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void og::type_checker::do_evaluation_node(og::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void og::type_checker::do_print_node(og::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void og::type_checker::do_read_node(og::read_node *const node, int lvl) {
  try {
    // node->argument()->accept(this, lvl);
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

//---------------------------------------------------------------------------

void og::type_checker::do_for_node(og::for_node *const node, int lvl) {
#if 0
  node->condition()->accept(this, lvl + 4);
#endif
}

//---------------------------------------------------------------------------

void og::type_checker::do_if_node(og::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void og::type_checker::do_if_else_node(og::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void og::type_checker::do_sizeof_node(og::sizeof_node *const node, int lvl) {
}

void og::type_checker::do_memory_reservation_node(og::memory_reservation_node *const node, int lvl) {
}

void og::type_checker::do_function_declaration_node(og::function_declaration_node *const node, int lvl) {
}

void og::type_checker::do_function_call_node(og::function_call_node *const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::type_checker::do_block_node(og::block_node *const node, int lvl) {
}

void og::type_checker::do_function_definition_node(og::function_definition_node *const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::type_checker::do_break_node(og::break_node *const node, int lvl) {
}

void og::type_checker::do_continue_node(og::continue_node * const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::type_checker::do_address_of_node(og::address_of_node* const node, int lvl) {
}

void og::type_checker::do_nullptr_node(og::nullptr_node* const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::make_reference_type(4, cdk::make_primitive_type(0, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void og::type_checker::do_return_node(og::return_node* const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::type_checker::do_variable_declaration_node(og::variable_declaration_node* const node, int lvl) {
  if (_in_function) {
    /* TODO: local variable */
  } else {
    const auto &ids = node->identifiers();

    if (!node->is_auto()) {
      std::string id = *ids->at(0);
      std::shared_ptr<og::symbol> symbol = _symtab.find(id);

      if (symbol == nullptr) {
        symbol = std::make_shared<og::symbol>(node->varType(), id);
        symbol->global(true);
        symbol->qualifier(node->qualifier());
        _symtab.insert(symbol->name(), symbol);
        _parent->set_new_symbol(symbol);
      } else {
        check_variable_declaration(node, symbol);
      }

      if (node->initializer()) {
        node->initializer()->accept(this, lvl + 2);
        check_variable_definition(node, symbol);
        symbol->defined(true);
      }
    } else {
      /* TODO: tuple declaration */
    }
  }
}

void og::type_checker::check_variable_declaration(og::variable_declaration_node *const node, std::shared_ptr<og::symbol> symbol) {
  if (symbol->is_function())
  {
    std::ostringstream oss;
    oss << "Redeclaration of function '" << symbol->name() << "' as variable";
    throw oss.str();
  }
  else if (!deep_type_check(symbol->type(), node->varType()))
  {
    std::ostringstream oss;
    oss << "Redeclaration of variable '" << symbol->name() << "' with different types: ";
    oss << cdk::to_string(symbol->type()) << " and ";
    oss << cdk::to_string(node->varType());
    throw oss.str();
  }
  else if (symbol->qualifier() != node->qualifier())
  {
    throw std::string("Redeclaration of variable with different qualifier");
  }
}

void og::type_checker::check_variable_definition(og::variable_declaration_node *const node, std::shared_ptr<og::symbol> symbol) {
  if (symbol->defined())
  {
    std::ostringstream oss;
    oss << "Redefinition of variable '" << symbol->name() << "'";
    throw oss.str();
  }
  else if (!assignment_compatible(symbol->type(), node->initializer()->type()))
  {
    std::ostringstream oss;
    oss << "Wrong types for definition: ";
    oss << cdk::to_string(symbol->type()) << " and ";
    oss << cdk::to_string(node->initializer()->type());
    throw oss.str();
  }
}

bool og::type_checker::assignment_compatible(std::shared_ptr<cdk::basic_type> l, std::shared_ptr<cdk::basic_type> r) {
  return l == r ||
         (l->name() == cdk::TYPE_DOUBLE && assignment_compatible(cdk::make_primitive_type(4, cdk::TYPE_INT), r)) ||
         (l->name() == cdk::TYPE_INT && r->name() == cdk::TYPE_POINTER && cdk::reference_type_cast(r)->referenced()->name() == cdk::TYPE_VOID);
}
//---------------------------------------------------------------------------

void og::type_checker::do_pointer_index_node(og::pointer_index_node* const node, int lvl) {
}

void og::type_checker::do_tuple_index_node(og::tuple_index_node* const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::type_checker::do_tuple_node(og::tuple_node* const node, int lvl) {
}
