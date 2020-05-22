#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "ast/all.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#include "og_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

bool og::type_checker::deep_type_check(std::shared_ptr<cdk::basic_type> l, std::shared_ptr<cdk::basic_type> r) {
  while (is_typed(l, cdk::TYPE_POINTER) && is_typed(r, cdk::TYPE_POINTER))
  {
    l = referenced(l);
    r = referenced(r);
  }

  if (l->name() == cdk::TYPE_STRUCT && cdk::structured_type_cast(l)->length() == 1) {
    return deep_type_check(cdk::structured_type_cast(l)->component(0), r);
  } else if (r->name() == cdk::TYPE_STRUCT && cdk::structured_type_cast(r)->length() == 1) {
    return deep_type_check(l, cdk::structured_type_cast(r)->component(0));
  } else if (l->name() == cdk::TYPE_STRUCT && r->name() == cdk::TYPE_STRUCT) {
    auto ls = cdk::structured_type_cast(l);
    auto rs = cdk::structured_type_cast(r);
    if (ls->length() != rs->length()) return false;

    for (size_t i = 0; i < ls->length(); i++) {
      if (!deep_type_check(ls->component(i), rs->component(i))) return false;
    }
    return true;
  } else {
    return l->name() == r->name();
  }
}

bool og::is_typed(std::shared_ptr<cdk::basic_type> type, cdk::typename_type name) {
  if (type->name() == cdk::TYPE_STRUCT && cdk::structured_type_cast(type)->length() == 1) {
    return is_typed(cdk::structured_type_cast(type)->component(0), name);
  }
  return type->name() == name;
}

std::shared_ptr<cdk::basic_type> og::referenced(std::shared_ptr<cdk::basic_type> type) {
  if (type->name() == cdk::TYPE_STRUCT) {
    type = cdk::structured_type_cast(type)->component(0);
  }
  return cdk::reference_type_cast(type)->referenced();
}

bool og::is_void_pointer(std::shared_ptr<cdk::basic_type> type) {
  if (!is_typed(type, cdk::TYPE_POINTER)) return false;
  while (is_typed(type, cdk::TYPE_POINTER)) {
    type = referenced(type);
  }
  return is_typed(type, cdk::TYPE_VOID);
}

void og::type_checker::hint_type(std::shared_ptr<cdk::basic_type> lt, cdk::typed_node *const r) {
  auto rt = r->type();
  if (is_typed(lt, cdk::TYPE_POINTER) && is_typed(rt, cdk::TYPE_POINTER) // memory reservation hint
      && is_typed(cdk::reference_type_cast(rt)->referenced(), cdk::TYPE_UNSPEC)) {
    r->type(lt);
  }
  else if ((is_typed(lt, cdk::TYPE_INT) || is_typed(lt, cdk::TYPE_DOUBLE)) && // input hint
            is_typed(rt, cdk::TYPE_UNSPEC)) {
    r->type(lt);
  }
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
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);
  if (node->argument()->type()->name() != cdk::TYPE_INT) throw std::string("integer expression expected for not operator");

  node->type(node->argument()->type());
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
  if (! (is_typed(node->argument()->type(), cdk::TYPE_INT) ||
        is_typed(node->argument()->type(), cdk::TYPE_DOUBLE)) ) {
    throw std::string("integer or real expression expected by symmetry operator");
  }
}

void og::type_checker::do_identity_node(og::identity_node *const node, int lvl) {
  ASSERT_UNSPEC;
  processUnaryExpression(node, lvl);
  if (! (is_typed(node->argument()->type(), cdk::TYPE_INT) ||
        is_typed(node->argument()->type(), cdk::TYPE_DOUBLE)) ) {
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

  if (!(is_typed(node->left()->type(), cdk::TYPE_INT) && is_typed(node->right()->type(), cdk::TYPE_INT))) {
    binaryOperationTypeError(node);
  }

  node->type(node->left()->type());
}

void og::type_checker::processMultiplicationDivision(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(is_typed(node->left()->type(), cdk::TYPE_INT)  || is_typed(node->left()->type(), cdk::TYPE_DOUBLE))  ||
      !(is_typed(node->right()->type(), cdk::TYPE_INT) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE))) {
        binaryOperationTypeError(node);
      }

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE)) {
    node->type(node->left()->type());
  } else if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
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

  if (!(is_typed(node->left()->type(), cdk::TYPE_INT)  || is_typed(node->left()->type(), cdk::TYPE_DOUBLE)) ||
      !(is_typed(node->right()->type(), cdk::TYPE_INT) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE))) {
        binaryOperationTypeError(node);
    }

  if (is_typed(node->left()->type(), cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (is_typed(node->right()->type(), cdk::TYPE_INT)) {
    node->type(node->right()->type());
  } else {
    node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
  }
}

void og::type_checker::processEquality(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if ((is_typed(node->left()->type(), cdk::TYPE_DOUBLE)  && is_typed(node->right()->type(), cdk::TYPE_POINTER)) ||
      (is_typed(node->left()->type(), cdk::TYPE_POINTER) && is_typed(node->right()->type(), cdk::TYPE_DOUBLE))) {
        binaryOperationTypeError(node);
      }

  if (is_typed(node->left()->type(), cdk::TYPE_INT)) {
    node->type(node->left()->type());
  } else if (is_typed(node->right()->type(), cdk::TYPE_INT)) {
    node->type(node->right()->type());
  } else {
    node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
  }
}

void og::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  node->right()->accept(this, lvl + 2);

  if (!(is_typed(node->left()->type(), cdk::TYPE_INT)  || is_typed(node->left()->type(), cdk::TYPE_DOUBLE)  || is_typed(node->left()->type(), cdk::TYPE_POINTER))  ||
      !(is_typed(node->right()->type(), cdk::TYPE_INT) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_POINTER)) ||
      (is_typed(node->left()->type(), cdk::TYPE_POINTER) && is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) ||
      (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && is_typed(node->right()->type(), cdk::TYPE_POINTER)) ||
      (is_typed(node->left()->type(), cdk::TYPE_POINTER) && is_typed(node->right()->type(), cdk::TYPE_POINTER))) {
        binaryOperationTypeError(node);
  }

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE)) {
    node->type(node->left()->type());
  } else if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    node->type(node->right()->type());
  } else if (is_typed(node->left()->type(), cdk::TYPE_POINTER)) {
    node->type(node->left()->type());
  } else if (is_typed(node->right()->type(), cdk::TYPE_POINTER)) {
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

  if (!(is_typed(node->left()->type(), cdk::TYPE_INT)  || is_typed(node->left()->type(), cdk::TYPE_DOUBLE)  || is_typed(node->left()->type(), cdk::TYPE_POINTER))  ||
      !(is_typed(node->right()->type(), cdk::TYPE_INT) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_POINTER)) ||
      (is_typed(node->left()->type(), cdk::TYPE_POINTER) && is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) ||
      (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && is_typed(node->right()->type(), cdk::TYPE_POINTER))) {
        binaryOperationTypeError(node);
  }

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE)) {
    node->type(node->left()->type());
  } else if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    node->type(node->right()->type());
  } else if (is_typed(node->left()->type(), cdk::TYPE_POINTER) && is_typed(node->right()->type(), cdk::TYPE_POINTER)) {
    if (!deep_type_check(node->left()->type(), node->right()->type()))
      binaryOperationTypeError(node);

    node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
  } else if (is_typed(node->left()->type(), cdk::TYPE_POINTER)) {
    node->type(node->left()->type());
  } else if (is_typed(node->right()->type(), cdk::TYPE_POINTER)) {
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

  if (!(is_typed(node->left()->type(), cdk::TYPE_INT) && is_typed(node->right()->type(), cdk::TYPE_INT))) {
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

  if (symbol == nullptr) {
    throw "Use of undeclared variable: " + id;
  }
  node->type(symbol->type());
}

void og::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl);
  node->type(node->lvalue()->type());
}

void og::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->lvalue()->accept(this, lvl);
  node->rvalue()->accept(this, lvl + 2);

  hint_type(node->lvalue()->type(), node->rvalue());

  if (!assignment_compatible(node->lvalue()->type(), node->rvalue()->type())) {
    throw "Incompatible types in assignment: " + cdk::to_string(node->lvalue()->type()) + " and " +
          cdk::to_string(node->rvalue()->type());
  }

  node->type(node->lvalue()->type());
}

//---------------------------------------------------------------------------

void og::type_checker::do_evaluation_node(og::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void og::type_checker::do_print_node(og::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
  auto int_hint = cdk::make_primitive_type(4, cdk::TYPE_INT);
  for (size_t i = 0; i < node->arguments()->size(); i++) {
    hint_type(int_hint, (cdk::expression_node*)node->arguments()->node(i));
  }
}

//---------------------------------------------------------------------------

void og::type_checker::do_read_node(og::read_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::make_primitive_type(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void og::type_checker::do_for_node(og::for_node *const node, int lvl) {
  _symtab.push();
  if (node->inits()) node->inits()->accept(this, lvl + 2);
  if (node->condition()) node->condition()->accept(this, lvl + 2);
  if (node->incrs()) node->incrs()->accept(this, lvl + 2);
  _symtab.pop();
}

//---------------------------------------------------------------------------

void og::type_checker::do_if_node(og::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  node->block()->accept(this, lvl + 4);
}

void og::type_checker::do_if_else_node(og::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  node->thenblock()->accept(this, lvl + 4);
  node->elseblock()->accept(this, lvl + 4);
}

void og::type_checker::do_sizeof_node(og::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->tuple()->accept(this, lvl + 4);
  node->type(cdk::make_primitive_type(4, cdk::TYPE_INT));
}

void og::type_checker::do_memory_reservation_node(og::memory_reservation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!is_typed(node->argument()->type(), cdk::TYPE_INT)) {
    throw std::string("Integer expression expected in memory reservation expression");
  }
  auto unspec_type = cdk::make_primitive_type(0, cdk::TYPE_UNSPEC);
  auto reference_type = cdk::make_reference_type(4, unspec_type);
  node->type(reference_type);
}

void og::type_checker::do_function_declaration_node(og::function_declaration_node *const node, int lvl) {
  std::string id;

  if (node->identifier() == "og") {
    id = "_main";
  } else if (node->identifier() == "_main") {
    id = "._main";
  } else {
    id = node->identifier();
  }

  std::vector<std::shared_ptr<cdk::basic_type>> param_types;

  _symtab.push(); // Don't polute the global scope
  _in_args = true;
  cdk::sequence_node *parameters = node->parameters();
  if (parameters != nullptr) {
    parameters->accept(this, lvl + 2);
    for(size_t i = 0; i < parameters->size(); i++) {
      param_types.push_back(((cdk::typed_node*)parameters->node(i))->type());
      _parent->pop_symbol();
    }
  }
  _in_args = false;
  _symtab.pop();

  std::shared_ptr<og::symbol> function = std::make_shared<og::symbol>(
      node->type(),
      id,
      0,
      node->qualifier(),
      true,
      false,
      true,
      param_types
      );
  std::shared_ptr<og::symbol> previous = _symtab.find(function->name());

  if (previous) {
    if (function->params().size() == previous->params().size()) {
      if (function->params().size()) {
        check_function_declaration(node, previous);
        for(size_t i = 0; i < parameters->size(); i++) {
          if (!deep_type_check(function->params().at(i), previous->params().at(i))) {
            throw std::string("Conflicting declaration for '" + node->identifier() + "'");
          }
        }
      }
    } else {
      std::ostringstream oss;
      oss << "Function '" << node->identifier() << "' first declared with ";
      oss << function->params().size() << " parameters then with " << previous->params().size();
      throw oss.str();
    }
    _parent->push_symbol(nullptr);
  } else {
    _symtab.insert(function->name(), function);
    _parent->push_symbol(function);
  }
}

void og::type_checker::check_function_declaration(og::function_declaration_node *const node, std::shared_ptr<og::symbol> symbol) {
  if (!symbol->is_function()) {
    std::ostringstream oss;
    oss << "Redeclaration of variable '" << symbol->name() << "' as function";
    throw oss.str();
  }
  else if (!deep_type_check(symbol->type(), node->type()))
  {
    std::ostringstream oss;
    oss << "Redeclaration of function '" << symbol->name() << "' with different types: ";
    oss << cdk::to_string(symbol->type()) << " and ";
    oss << cdk::to_string(node->type());
    throw oss.str();
  }
  else if (symbol->qualifier() != node->qualifier())
  {
    throw std::string("Redeclaration of function with different qualifier");
  }
}

void og::type_checker::do_function_call_node(og::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;

  std::string id;
  if (node->identifier() == "og") {
    id = "_main";
  } else if (node->identifier() == "_main") {
    id = "._main";
  } else {
    id = node->identifier();
  }

  std::shared_ptr<og::symbol> function = _symtab.find(id);

  if (function == NULL) {
    throw std::string("function '" + node->identifier() + "'is undeclared");
  }

  if (!function->is_function()) {
    throw std::string("symbol '" + node->identifier() + "'is not a function");
  }

  if (function->params().size() == node->arguments()->size()) {
    if (function->params().size()) {
      node->arguments()->accept(this, lvl + 2);
      cdk::sequence_node* args = node->arguments();
      std::vector<std::shared_ptr<cdk::basic_type>> &params = function->params();

      for (size_t i = 0; i < params.size(); ++i) {
        auto arg = (cdk::expression_node*)args->node(i);
        std::shared_ptr<cdk::basic_type> param_type = params[i];

        hint_type(param_type, arg);

        if (!assignment_compatible(param_type, arg->type())) {
          std::ostringstream oss;
          oss << "function '" << node->identifier() << "' expected argument of type `";
          oss << to_string(param_type) << "` but got `" << to_string(arg->type()) << "` instead";
          throw oss.str();
        }
      }
    }
  }
  else {
    std::ostringstream oss;
    oss << "function '" << node->identifier() << "' expected ";
    oss << function->params().size() << " arguments but got " << node->arguments()->size();
    throw oss.str();
  }
  node->type(function->type());
}

//---------------------------------------------------------------------------

void og::type_checker::do_block_node(og::block_node *const node, int lvl) {
  _symtab.push();
  if (node->declarations()) node->declarations()->accept(this, lvl + 2);
  if (node->instructions()) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

void og::type_checker::do_function_definition_node(og::function_definition_node *const node, int lvl) {
  std::string id;
  if (node->identifier() == "og") {
    id = "_main";
  } else if (node->identifier() == "_main") {
    id = "._main";
  } else {
    id = node->identifier();
  }

  std::vector<std::shared_ptr<cdk::basic_type>> param_types;
  cdk::sequence_node *parameters = node->parameters();

  std::shared_ptr<og::symbol> function = std::make_shared<og::symbol>(
      node->type(),
      id,
      0,
      node->qualifier(),
      true,
      true,
      true,
      param_types
      );

  std::shared_ptr<og::symbol> previous = _symtab.find(function->name());
  if (previous) {
    if (!previous->is_function()) {
      std::ostringstream oss;
      oss << "Redeclaration of variable '" << previous->name() << "' as function";
      throw oss.str();
    }
    if (previous->defined()) {
      std::ostringstream oss;
      oss << "Redefinition of function '" << node->identifier() << "'";
      throw oss.str();
    }
    if (!deep_type_check(previous->type(), node->type())) {
      std::ostringstream oss;
      oss << "Definition of function '" << previous->name() << "' with different type from declaration: ";
      oss << cdk::to_string(previous->type()) << " and ";
      oss << cdk::to_string(node->type());
      throw oss.str();
    }
    if (previous->qualifier() != node->qualifier())
    {
      throw std::string("Definition of function with different qualifier from declaration");
    }
  }

  if (previous) {
    _symtab.replace(function->name(), function);
  }
  else {
    _symtab.insert(function->name(), function);
  }

  _offset = 8;
  _in_args = true;
  _symtab.push();
  if (node->parameters()) {
    node->parameters()->accept(this, lvl + 2);
    for(size_t i = 0; i < parameters->size(); i++) {
      param_types.push_back(((cdk::typed_node*)parameters->node(i))->type());
    }
    function->params(param_types);
  }
  _in_args = false;

  if (previous) {
    if (function->params().size() == previous->params().size()) {
      if (function->params().size()) {
        for(size_t i = 0; i < parameters->size(); i++) {
          if (!deep_type_check(function->params().at(i), previous->params().at(i))) {
            throw std::string("Conflicting definition for '" + node->identifier() + "'");
          }
        }
      }
    } else {
      std::ostringstream oss;
      oss << "Function '" << node->identifier() << "' first declared with ";
      oss << function->params().size() << " parameters then with " << previous->params().size();
      throw oss.str();
    }
  }

  _offset = 0;
  _function = function;
  node->block()->accept(this, lvl + 2);
  _symtab.pop();
}

//---------------------------------------------------------------------------

void og::type_checker::do_break_node(og::break_node *const node, int lvl) {
}

void og::type_checker::do_continue_node(og::continue_node * const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::type_checker::do_address_of_node(og::address_of_node* const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::make_reference_type(4, node->lvalue()->type()));
}

void og::type_checker::do_nullptr_node(og::nullptr_node* const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::make_reference_type(4, cdk::make_primitive_type(0, cdk::TYPE_VOID)));
}

//---------------------------------------------------------------------------

void og::type_checker::do_return_node(og::return_node* const node, int lvl) {
  ASSERT_UNSPEC;

  if (node->retval()) {
    // return exprs;
    node->retval()->accept(this, lvl + 2);
    node->type(node->retval()->type());

    if (node->type()->name() != cdk::TYPE_STRUCT) {
      // non tuple return type
      if (_function->type()->name() == cdk::TYPE_UNSPEC) {
        _function->type(node->type());
        return;
      }
      if (assignment_compatible(_function->type(), node->type())) {
        // Do nothing
      }
      else if (assignment_compatible(node->type(), _function->type())) {
        _function->type(node->type());
      }
      else {
        throw "Incompatible types in return: " + cdk::to_string(node->type()) + " and " +
          cdk::to_string(_function->type());
      }
    }
    else {
      // tuple return type
      if (_function->type()->name() == cdk::TYPE_UNSPEC) {
        _function->type(node->type());
        return;
      }
      auto return_type = cdk::structured_type_cast(_function->type());
      auto node_type = cdk::structured_type_cast(node->type());
      if (return_type->length() != node_type->length()) {
        throw "Incompatible types in return: " + cdk::to_string(node_type) + " and " +
          cdk::to_string(return_type);
      }
      auto return_types = return_type->components();
      auto node_types = node_type->components();
      for (size_t i = 0; i < return_type->length(); ++i) {
        auto inner_ret_type = return_types[i];
        auto inner_node_type = node_types[i];
        if (assignment_compatible(inner_ret_type, inner_node_type)) {
          // Do nothing
        }
        else if (assignment_compatible(inner_node_type, inner_ret_type)) {
          return_types[i] = inner_node_type;
        }
        else {
          throw "Incompatible types in return: " + cdk::to_string(inner_node_type) + " and " +
            cdk::to_string(inner_ret_type);
        }
      }
    }
  }
  else {
    node->type(cdk::make_primitive_type(0, cdk::TYPE_VOID));
    if (_function->type()->name() == cdk::TYPE_UNSPEC) {
      _function->type(node->type());
    }
    else if (_function->type()->name() != cdk::TYPE_VOID) {
      throw std::string("Empty return for " + _function->name());
    }
  }
}

//---------------------------------------------------------------------------

void og::type_checker::do_variable_declaration_node(og::variable_declaration_node* const node, int lvl) {
  ASSERT_UNSPEC;
  const auto &ids = node->identifiers();
  if (_function || _in_args) {
    if (!node->is_auto()) {
      std::string id = *ids->at(0);

      std::shared_ptr<og::symbol> symbol = std::make_shared<og::symbol>(node->varType(), id);
      symbol->global(false);
      symbol->qualifier(node->qualifier());
      if(!_symtab.insert(symbol->name(), symbol))
        throw std::string("Redeclaration of local variable: ") + id;
      _parent->push_symbol(symbol);

      if (_in_args) {
        symbol->offset(_offset);
        _offset += node->varType()->size();
      }
      else {
        _offset += node->varType()->size();
        symbol->offset(-_offset);
      }

      node->type(node->varType());

      if (node->initializer()) {
        node->initializer()->accept(this, lvl + 2);

        hint_type(node->type(), node->initializer());

        check_variable_definition(node, symbol);
      }
    } else { // auto dec
      auto tuple = (og::tuple_node*)node->initializer();
      tuple->accept(this, lvl + 2);
      if (ids->size() > 1) { // explode tuple
        if (tuple->type()->name() != cdk::TYPE_STRUCT || ids->size() != cdk::structured_type_cast(tuple->type())->length()) {
          throw "Auto declaration with wrong number of elements: left " + std::to_string(ids->size()) +
                ", right " + (tuple->type()->name() == cdk::TYPE_STRUCT ? std::to_string(cdk::structured_type_cast(tuple->type())->length()) : "1");
        }
        for (size_t i = 0; i < ids->size(); i++) {
          std::string id = *ids->at(i);

          std::shared_ptr<cdk::basic_type> type = cdk::structured_type_cast(tuple->type())->component(i);
          std::shared_ptr<og::symbol> symbol = std::make_shared<og::symbol>(type, id);
          symbol->global(false);
          symbol->qualifier(node->qualifier());
          if(!_symtab.insert(symbol->name(), symbol))
            throw std::string("Redeclaration of local variable: ") + id;
          _parent->push_symbol(symbol);
          _offset += type->size();
          symbol->offset(-_offset);
        }
      } else { // non-explosion
          std::string id = *ids->at(0);
          std::shared_ptr<cdk::basic_type> type = ((cdk::expression_node*)tuple)->type();
          std::shared_ptr<og::symbol> symbol = std::make_shared<og::symbol>(type, id);
          symbol->global(false);
          symbol->qualifier(node->qualifier());
          if(!_symtab.insert(symbol->name(), symbol))
            throw std::string("Redeclaration of local variable: ") + id;
          _parent->push_symbol(symbol);
          _offset += type->size();
          symbol->offset(-_offset);
      }
      node->type(tuple->type());
    }
  } else { // outside function
    if (!node->is_auto()) {
      std::string id = *ids->at(0);
      std::shared_ptr<og::symbol> symbol = _symtab.find(id);

      if (symbol == nullptr) {
        symbol = std::make_shared<og::symbol>(node->varType(), id);
        symbol->global(true);
        symbol->qualifier(node->qualifier());
        _symtab.insert(symbol->name(), symbol);
      } else {
        check_variable_declaration(node, symbol, node->varType());
      }
      _parent->push_symbol(symbol);

      if (node->initializer()) {
        node->initializer()->accept(this, lvl + 2);
        check_variable_definition(node, symbol);
        symbol->defined(true);
      }
      node->type(node->varType());
    } else { // auto dec
      auto tuple = (og::tuple_node*)node->initializer();
      tuple->accept(this, lvl + 2);
      if (ids->size() > 1) { // explode tuple
        if (ids->size() != tuple->members()->size()) {
          throw "Auto declaration with wrong number of elements: left " + std::to_string(ids->size()) +
                ", right " + std::to_string(tuple->members()->size());
        }
        for (size_t i = 0; i < ids->size(); i++) {
          std::string id = *ids->at(i);
          std::shared_ptr<og::symbol> symbol = _symtab.find(id);
          std::shared_ptr<cdk::basic_type> type = ((cdk::expression_node*)tuple->members()->node(i))->type();

          if (symbol == nullptr) {
            symbol = std::make_shared<og::symbol>(type, id);
            symbol->global(true);
            symbol->qualifier(node->qualifier());
            _symtab.insert(symbol->name(), symbol);
          } else {
            check_variable_declaration(node, symbol, type);
          }
          _parent->push_symbol(symbol);

          node->initializer()->accept(this, lvl + 2);
          check_variable_definition(node, symbol);
          symbol->defined(true);
        }
      } else { // non-explosion
          std::string id = *ids->at(0);
          std::shared_ptr<og::symbol> symbol = _symtab.find(id);
          std::shared_ptr<cdk::basic_type> type = ((cdk::expression_node*)tuple)->type();
          if (symbol == nullptr) {
            symbol = std::make_shared<og::symbol>(type, id);
            symbol->global(true);
            symbol->qualifier(node->qualifier());
            _symtab.insert(symbol->name(), symbol);
          } else {
            check_variable_declaration(node, symbol, type);
          }
          _parent->push_symbol(symbol);

          node->initializer()->accept(this, lvl + 2);
          check_variable_definition(node, symbol);
          symbol->defined(true);
      }
      node->type(tuple->type());
    }
  }
}

void og::type_checker::check_variable_declaration(og::variable_declaration_node *const node, std::shared_ptr<og::symbol> symbol, std::shared_ptr<cdk::basic_type> type) {
  if (symbol->is_function())
  {
    std::ostringstream oss;
    oss << "Redeclaration of function '" << symbol->name() << "' as variable";
    throw oss.str();
  }
  else if (!deep_type_check(symbol->type(), type))
  {
    std::ostringstream oss;
    oss << "Redeclaration of variable '" << symbol->name() << "' with different types: ";
    oss << cdk::to_string(symbol->type()) << " and ";
    oss << cdk::to_string(type);
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
  else if (!node->varType()->name() == cdk::TYPE_UNSPEC && !assignment_compatible(symbol->type(), node->initializer()->type()))
  {
    std::ostringstream oss;
    oss << "Wrong types for definition: ";
    oss << cdk::to_string(symbol->type()) << " and ";
    oss << cdk::to_string(node->initializer()->type());
    throw oss.str();
  }
}

bool og::type_checker::assignment_compatible(std::shared_ptr<cdk::basic_type> l, std::shared_ptr<cdk::basic_type> r) {
  return deep_type_check(l, r) ||
         (is_typed(l, cdk::TYPE_DOUBLE) && is_typed(r, cdk::TYPE_INT)) ||
         (is_typed(l, cdk::TYPE_INT) && is_void_pointer(r)) ||
         (is_typed(l, cdk::TYPE_POINTER) && is_void_pointer(r)) ||
         (is_typed(r, cdk::TYPE_POINTER) && is_void_pointer(l));
}
//---------------------------------------------------------------------------

void og::type_checker::do_pointer_index_node(og::pointer_index_node* const node, int lvl) {
  ASSERT_UNSPEC;
  node->base()->accept(this, lvl + 2);
  if (!is_typed(node->base()->type(), cdk::TYPE_POINTER)) {
    throw std::string("Illegal attempt to index non-pointer expression");
  }
  auto reference_type = cdk::reference_type_cast(node->base()->type());
  node->type(reference_type->referenced());
  node->index()->accept(this, lvl + 2);
  if (!is_typed(node->index()->type(), cdk::TYPE_INT)) {
    throw std::string("Non integer index for pointer-indexing");
  }
}

void og::type_checker::do_tuple_index_node(og::tuple_index_node* const node, int lvl) {
  ASSERT_UNSPEC;
  node->tuple()->accept(this, lvl + 2);
  if (!is_typed(node->tuple()->type(), cdk::TYPE_STRUCT)) {
    throw std::string("Illegal attempt to index non-tuple expression");
  }

  size_t idx = node->index();
  auto structured_type = cdk::structured_type_cast(node->tuple()->type());
  if (idx < 1 || idx > structured_type->length()) {
    throw std::string("Out of bounds access in tuple");
  }
  node->type(structured_type->component(idx-1));
}

//---------------------------------------------------------------------------

void og::type_checker::do_tuple_node(og::tuple_node* const node, int lvl) {
  ASSERT_UNSPEC;
  std::vector<std::shared_ptr<cdk::basic_type>> types;
  node->members()->accept(this, lvl + 2);

  auto int_hint = cdk::make_primitive_type(4, cdk::TYPE_INT); // hint for input inside tuple
  for (size_t i = 0; i < node->members()->size(); i++) {
    hint_type(int_hint, (cdk::expression_node*)node->members()->node(i));
  }

  if (node->members()->size() == 1) { // avoid creating unit tuple
    node->type(((cdk::expression_node*)node->members()->node(0))->type());
  } else {
    for (size_t i = 0; i < node->members()->size(); i++) {
      types.push_back((((cdk::expression_node*)node->members()->node(i))->type()));
    }
    node->type(cdk::make_structured_type(types));
  }
}
