#include <string>
#include <sstream>
#include "targets/frame_size_calculator.h"
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "ast/all.h"  // all.h is automatically generated

#include "og_parser.tab.h"

//---------------------------------------------------------------------------

void og::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void og::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void og::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  _pf.DOUBLE(node->value());
}

void og::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int zero = ++_lbl;
  int end = ++_lbl;
  node->argument()->accept(this, lvl + 2);
  _pf.JZ(mklbl(zero));

  // If it was not 0 it becomes 0
  _pf.INT(0);
  _pf.JMP(mklbl(end));

  // If it was 0 it becomes 1
  _pf.LABEL(mklbl(zero));
  _pf.INT(1);

  _pf.LABEL(mklbl(end));
}

void og::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.LABEL(mklbl(lbl));
}

void og::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
  if (!_function) { // after all declarations
    // declare external functions
    for (std::string s: _functions_to_declare) {
      auto symbol = _symtab.find(s);
        if (!symbol || !symbol->defined()) {
        _pf.EXTERN(s);
      }
    }

    if (_uninitialized_vars.size() > 0) {
      _pf.BSS();
      _pf.ALIGN();
      for (std::string s : _uninitialized_vars)
      {
        auto symbol = _symtab.find(s);
        if (symbol->qualifier() == tREQUIRE) {
          _pf.EXTERN(symbol->name());
        } else {
          if (symbol->qualifier() == tPUBLIC) {
            _pf.GLOBAL(symbol->name(), _pf.OBJ());
          }
          _pf.LABEL(symbol->name());
          _pf.SALLOC(symbol->type()->size());
        }
      }
    }
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  _pf.INT(node->value()); // push an integer
}

void og::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  /* leave the address on the stack */
  _pf.TEXT(); // return to the TEXT segment
  _pf.ADDR(mklbl(lbl1)); // the string to be printed
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

void og::postfix_writer::do_identity_node(og::identity_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  } else if (is_typed(node->right()->type(), cdk::TYPE_POINTER)) {
    auto t = referenced(node->right()->type());
    if (t->name() != cdk::TYPE_VOID) {
      _pf.INT(t->size());
      _pf.MUL();
    }
  }

  node->right()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  } else if (is_typed(node->left()->type(), cdk::TYPE_POINTER)) {
    auto t = referenced(node->left()->type());
    if (t->name() != cdk::TYPE_VOID) {
      _pf.INT(t->size());
      _pf.MUL();
    }
  }

  if (is_typed(node->type(), cdk::TYPE_DOUBLE))
    _pf.DADD();
  else
    _pf.ADD();
}
void og::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  } else if (!is_typed(node->left()->type(), cdk::TYPE_POINTER) && is_typed(node->right()->type(), cdk::TYPE_POINTER)) {
    auto t = referenced(node->right()->type());
    if (t->name() != cdk::TYPE_VOID) {
      _pf.INT(t->size());
      _pf.MUL();
    }
  }

  node->right()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  } else if (!is_typed(node->right()->type(), cdk::TYPE_POINTER) && is_typed(node->left()->type(), cdk::TYPE_POINTER)) {
    auto t = referenced(node->left()->type());
    if (t->name() != cdk::TYPE_VOID) {
      _pf.INT(t->size());
      _pf.MUL();
    }
  }

  if (is_typed(node->type(), cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
    if (is_typed(node->left()->type(), cdk::TYPE_POINTER) && is_typed(node->right()->type(), cdk::TYPE_POINTER)) {
      auto t = referenced(node->left()->type());
      if (t->name() != cdk::TYPE_VOID) {
        _pf.INT(t->size());
        _pf.DIV();
      }
    }
  }
}
void og::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->type(), cdk::TYPE_DOUBLE))
    _pf.DMUL();
  else
    _pf.MUL();
}
void og::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->type(), cdk::TYPE_DOUBLE))
    _pf.DDIV();
  else
    _pf.DIV();
}
void og::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void og::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
  _pf.LT();
}
void og::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
  _pf.LE();
}
void og::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
  _pf.GE();
}
void og::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
  _pf.GT();
}
void og::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
  _pf.NE();
}
void og::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (is_typed(node->right()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->left()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  node->right()->accept(this, lvl);
  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) && !is_typed(node->right()->type(), cdk::TYPE_DOUBLE))
    _pf.I2D();

  if (is_typed(node->left()->type(), cdk::TYPE_DOUBLE) || is_typed(node->right()->type(), cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
  _pf.EQ();
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find(node->name());
  if (symbol->global())
    _pf.ADDR(node->name());
  else
    _pf.LOCAL(symbol->offset());
}

void og::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (is_typed(node->type(), cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else if (is_typed(node->type(), cdk::TYPE_STRUCT)) {
    /* do nothing */
    /* but this prevents unit tuples from being indexed */
  } else if (is_typed(node->type(), cdk::TYPE_VOID)) {
    _pf.TRASH(4);
  } else {
    _pf.LDINT();
  }
}

void og::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl); // determine the new value
  if (is_typed(node->lvalue()->type(), cdk::TYPE_DOUBLE)) {
    if (!is_typed(node->rvalue()->type(), cdk::TYPE_DOUBLE)) {
      _pf.I2D();
    }
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl); // where to store the value
  if (is_typed(node->lvalue()->type(), cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_evaluation_node(og::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  if (node->argument()->type()->size()) {
    _pf.TRASH(node->argument()->type()->size());
  }
}

void og::postfix_writer::do_print_node(og::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  cdk::expression_node* argument;
  for (size_t i = 0; i < node->arguments()->size(); ++i) {
    argument = (cdk::expression_node*) node->arguments()->node(i);
    argument->accept(this, lvl); // determine the value to print

    if (is_typed(argument->type(), cdk::TYPE_INT)) {
      _functions_to_declare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // delete the printed value
    } else if (is_typed(argument->type(), cdk::TYPE_DOUBLE)) {
      _functions_to_declare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // delete the printed value
    } else if (is_typed(argument->type(), cdk::TYPE_STRING)) {
      _functions_to_declare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // delete the printed value's address
    } else {
      std::cerr << "ERROR: Can only print ints, doubles and strings!" << std::endl;
      exit(1);
    }

  }

  if(node->newline()) {
    _functions_to_declare.insert("println");
    _pf.CALL("println"); // print a newline
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_read_node(og::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (is_typed(node->type(), cdk::TYPE_DOUBLE)) {
    _functions_to_declare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else {
    _functions_to_declare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_for_node(og::for_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int old_for_incr = _for_incr;
  int old_for_end = _for_end;

  int for_cond = ++_lbl;
  _for_incr = ++_lbl;
  _for_end = ++_lbl;

  _symtab.push();
  if (node->inits()) node->inits()->accept(this, lvl + 2);


  _pf.LABEL(mklbl(for_cond));
  if (node->condition()) {
    for (size_t i = 0; i < node->condition()->size(); i++) {
      cdk::expression_node *expr = (cdk::expression_node *)node->condition()->node(i);
      expr->accept(this, lvl + 4);
      if (i == node->condition()->size() - 1) {
        if (is_typed(expr->type(), cdk::TYPE_DOUBLE)) _pf.D2I();
      } else {
        _pf.TRASH(expr->type()->size());
      }
    }
  } else {
    _pf.INT(1);
  }
  _pf.JZ(mklbl(_for_end));

  node->block()->accept(this, lvl + 2);

  _pf.LABEL(mklbl(_for_incr));
  if (node->incrs()) {
    node->incrs()->accept(this, lvl + 2);
    size_t total = 0;
    for (size_t i = 0; i < node->incrs()->size(); i++) {
      total += ((cdk::expression_node*)node->incrs()->node(i))->type()->size();
    }
    _pf.TRASH(total);
  }
  _pf.JMP(mklbl(for_cond));

  _pf.LABEL(mklbl(_for_end));

  _symtab.pop();

  _for_incr = old_for_incr;
  _for_end = old_for_end;
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_if_node(og::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_if_else_node(og::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

void og::postfix_writer::do_sizeof_node(og::sizeof_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->tuple()->type()->size());
}

void og::postfix_writer::do_memory_reservation_node(og::memory_reservation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  auto reference_type = cdk::reference_type_cast(node->type());
  auto referenced_type = reference_type->referenced();
  if (is_typed(referenced_type, cdk::TYPE_UNSPEC)) {
    std::cerr << (node)->lineno() << ": " << "Unspecified pointer type for memory reservation" << std::endl; \
  }
  int size = referenced_type->size();
  if (size) {
    _pf.INT(size);
    _pf.MUL();
  }
  _pf.ALLOC();
  _pf.SP();
}

void og::postfix_writer::do_function_declaration_node(og::function_declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::shared_ptr<og::symbol> function = pop_symbol();
  if (!function) return;

  _functions_to_declare.insert(function->name());
}

void og::postfix_writer::do_function_call_node(og::function_call_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::string id;
  if (node->identifier() == "og") {
    id = "_main";
  } else if (node->identifier() == "_main") {
    id = "._main";
  } else {
    id = node->identifier();
  }

  std::shared_ptr<og::symbol> function = _symtab.find(id);

  cdk::sequence_node* args = node->arguments();
  std::vector<std::shared_ptr<cdk::basic_type>> &params = function->params();

  size_t argsSize = 0;
  for (int idxAhead = args->size(); idxAhead > 0; --idxAhead) {
    auto arg = (cdk::expression_node*)args->node(idxAhead-1);
    std::shared_ptr<cdk::basic_type> param_type = params[idxAhead-1];
    arg->accept(this, lvl + 2);
    if (is_typed(param_type, cdk::TYPE_DOUBLE)) {
      if (!is_typed(arg->type(), cdk::TYPE_DOUBLE)) {
        _pf.I2D();
      }
    }
    argsSize += param_type->size();
  }
  if (is_typed(node->type(), cdk::TYPE_STRUCT)) {
    _pf.LOCAL(-_local_size);
    argsSize += 4;
  }

  _pf.CALL(function->name());
  if (argsSize) {
    _pf.TRASH(argsSize);
  }

  auto type_name = function->type()->name();
  if (type_name == cdk::TYPE_VOID) {
    // No value to fetch
  }
  else if (type_name == cdk::TYPE_DOUBLE) {
    _pf.LDFVAL64();
  }
  else {
    _pf.LDFVAL32();
  }
}

void og::postfix_writer::do_block_node(og::block_node *const node, int lvl) {
  _symtab.push();
  if (node->declarations()) node->declarations()->accept(this, lvl+2);
  if (node->instructions()) node->instructions()->accept(this, lvl+2);
  _symtab.pop();
}

void og::postfix_writer::do_function_definition_node(og::function_definition_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (_function->qualifier() == tPUBLIC) {
    _pf.GLOBAL(_function->name(), _pf.FUNC());
  }
  _pf.LABEL(_function->name());

  frame_size_calculator lsc(_compiler);
  node->accept(&lsc, lvl);
  _local_size = lsc.localsize();
  if (_local_size) {
    _pf.ENTER(_local_size);
  } else {
    _pf.START();
  }

  _symtab.push(); // args scope

  _in_args = true;
  if (node->parameters()) {
    node->parameters()->accept(this, lvl + 2);
  }
  _in_args = false;


  node->block()->accept(this, lvl+2);

  _symtab.pop();  // args scope

  _function = nullptr;
  _offset = 0;

  _pf.LEAVE();
  _pf.RET();
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_break_node(og::break_node* const node, int lvl) {
  if (_for_end == 0) {
    std::cerr << node->lineno() << ": " << "Use of break outside of for loop" << std::endl; \
    return;
  }

  _pf.JMP(mklbl(_for_end));
}

void og::postfix_writer::do_continue_node(og::continue_node * const node, int lvl) {
  if (_for_incr == 0) {
    std::cerr << node->lineno() << ": " << "Use of continue outside of for loop" << std::endl; \
    return;
  }

  _pf.JMP(mklbl(_for_incr));
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_address_of_node(og::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

void og::postfix_writer::do_nullptr_node(og::nullptr_node* const node, int lvl) {
  _pf.INT(0);
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_return_node(og::return_node* const node, int lvl) {
  if (_function->type()->name() == cdk::TYPE_VOID) {
    // Do nothing
  }
  else if (_function->type()->name() == cdk::TYPE_STRUCT) {
    auto return_type = cdk::structured_type_cast(_function->type());
    auto return_types = return_type->components();
    auto tuple = node->retval();
    // get pointer to tuple
    _pf.LOCAL(8);
    _pf.LDINT();
    // duplicate address, 1 is used for offsets, 1 will be returned
    _pf.DUP32();
    for (size_t i = 0; i < tuple->members()->size(); ++i) {
      // duplicate address, 1 is used to store this expr, 1 for next offsets
      _pf.DUP32();
      // evaluate expression
      auto expr = (cdk::expression_node*)tuple->members()->node(i);
      auto type = return_types[i];
      // store the value
      if (type->name() == cdk::TYPE_DOUBLE) {
        _pf.DUP32(); // fodder for swap
        expr->accept(this, lvl + 2);
        if (expr->type()->name() != cdk::TYPE_DOUBLE) {
          _pf.I2D();
        }
        _pf.SWAP64();
        _pf.TRASH(4); // destroy fodder
        _pf.STDOUBLE();
      }
      else{
        expr->accept(this, lvl + 2);
        _pf.SWAP32();
        _pf.STINT();
      }
      // increment saved address by the size of type
      _pf.INT(type->size());
      _pf.ADD();
    }
    _pf.TRASH(4);
    _pf.STFVAL32();
  }
  else if (_function->type()->name() == cdk::TYPE_DOUBLE) {
    node->retval()->accept(this, lvl + 2);
    auto tuple = node->retval();
    auto expr = (cdk::expression_node*)tuple->members()->node(0);
    if (expr->type()->name() != cdk::TYPE_DOUBLE) {
      _pf.I2D();
    }
    _pf.STFVAL64();
  }
  else {
    node->retval()->accept(this, lvl + 2);
    _pf.STFVAL32();
  }
  _pf.LEAVE();
  _pf.RET();
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_variable_declaration_node(og::variable_declaration_node* const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_in_args) {
    auto sym = pop_symbol();

    if (_function->type()->name() == cdk::TYPE_STRUCT)
      sym->offset(sym->offset() + 4);

    _symtab.insert(sym->name(), sym);
    return;
  }
  if (_function) {
    if (node->initializer()) {
      if (!node->is_auto()) {
        auto sym = pop_symbol();
        _symtab.insert(sym->name(), sym);
        int offset = sym->offset();
        node->initializer()->accept(this, lvl + 2);
        if (is_typed(node->varType(), cdk::TYPE_DOUBLE)) {
          if (!is_typed(node->initializer()->type(), cdk::TYPE_DOUBLE)) {
            _pf.I2D();
          }
          _pf.LOCAL(offset);
          _pf.STDOUBLE();
        }
        else {
          _pf.LOCAL(offset);
          _pf.STINT();
        }
      } else { // auto dec
        const auto &ids = node->identifiers();
        auto tuple = (og::tuple_node*)node->initializer();
        if (ids->size() > 1) { // explode tuple
          if (tuple->members()->size() == 1) { // func call
            tuple->members()->node(0)->accept(this, lvl + 2); // call
            int ret_offset = 0;
            for (size_t i = 0; i < cdk::structured_type_cast(tuple->type())->length(); i++) {
              auto sym = pop_symbol();
              _symtab.insert(sym->name(), sym);

              _pf.DUP32();
              if (ret_offset) {
                _pf.INT(ret_offset);
                _pf.ADD();
              }

              if (is_typed(sym->type(), cdk::TYPE_DOUBLE)) {
                _pf.LDDOUBLE();
                _pf.LOCAL(sym->offset());
                _pf.STDOUBLE();
              } else {
                _pf.LDINT();
                _pf.LOCAL(sym->offset());
                _pf.STINT();
              }

              ret_offset += sym->type()->size();
            }
            _pf.TRASH(4); // remove extra copy of tuple address
          } else { // list tuple
            for (size_t i = 0; i < tuple->members()->size(); i++) {
              auto sym = pop_symbol();
              int offset = sym->offset();
              _symtab.insert(sym->name(), sym);
              auto expr = ((cdk::expression_node*)tuple->members()->node(i));
              expr->accept(this, lvl + 2);

              if (is_typed(expr->type(), cdk::TYPE_DOUBLE)) {
                _pf.LOCAL(offset);
                _pf.STDOUBLE();
              }
              else {
                _pf.LOCAL(offset);
                _pf.STINT();
              }
            }
          }
        } else { // non-explosion
          auto sym = pop_symbol();
          int offset = sym->offset();
          _symtab.insert(sym->name(), sym);
          for (size_t i = 0; i < tuple->members()->size(); i++) {
            auto expr = ((cdk::expression_node*)tuple->members()->node(i));
            expr->accept(this, lvl + 2);

            if (is_typed(expr->type(), cdk::TYPE_STRUCT)) {
              for (size_t i = 0; i < expr->type()->size()/4; i++) {
                _pf.DUP32();
                if (i) {
                  _pf.INT(i*4);
                  _pf.ADD();
                }
                _pf.LDINT();
                _pf.LOCAL(offset + i*4);
                _pf.STINT();
              }
              _pf.TRASH(4);
            } else if (is_typed(expr->type(), cdk::TYPE_DOUBLE)) {
              _pf.LOCAL(offset);
              _pf.STDOUBLE();
            }
            else {
              _pf.LOCAL(offset);
              _pf.STINT();
            }

            offset += expr->type()->size();
          }
        }
      }
    } else { // local variable without initial value
      auto sym = pop_symbol();
      _symtab.insert(sym->name(), sym);
    }
  }
  else { // global
    if (!node->is_auto()) {
      auto sym = pop_symbol();
      if (sym) {
        _symtab.insert(sym->name(), sym);
        _uninitialized_vars.insert(sym->name());
      }
    }

    if (node->initializer()) {
      if (!node->is_auto()) {
        std::string id = *node->identifiers()->at(0);
        _uninitialized_vars.erase(id);
        if (is_typed(node->initializer()->type(), cdk::TYPE_STRING)) {
          int lbl;
          cdk::string_node *s = dynamic_cast<cdk::string_node *>(node->initializer());
          _pf.RODATA();
          _pf.ALIGN();
          _pf.LABEL(mklbl(lbl = ++_lbl));
          _pf.SSTRING(s->value());
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SADDR(mklbl(lbl));
        }
        else if (is_typed(node->varType(), cdk::TYPE_DOUBLE)) {
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          cdk::double_node *d = dynamic_cast<cdk::double_node *>(node->initializer());
          if (d) {
            _pf.SDOUBLE(d->value());
          } else {
            cdk::integer_node *i = dynamic_cast<cdk::integer_node *>(node->initializer());
            _pf.SDOUBLE(i->value());
          }
        }
        else if (is_typed(node->initializer()->type(), cdk::TYPE_POINTER)) {
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SINT(0); // only nullptr literal
        }
        else if (is_typed(node->initializer()->type(), cdk::TYPE_INT)) {
          cdk::integer_node *i = dynamic_cast<cdk::integer_node *>(node->initializer());
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SINT(i->value());
        }

      }
      else { // tuple
        const auto &ids = node->identifiers();
        auto tuple = (og::tuple_node*)node->initializer();
        std::string id;

        if (ids->size() == 1) { // non-explosion
          id = *ids->at(0);
          _pf.DATA();
          _pf.LABEL(id);
          _uninitialized_vars.erase(id);
          auto sym = pop_symbol();
          _symtab.insert(sym->name(), sym);
        }

        for (size_t i = 0; i < tuple->members()->size(); i++) {
          auto expr = ((cdk::expression_node*)tuple->members()->node(i));
          if (ids->size() != 1) { // <=> explosion
            id = *ids->at(i);
            auto sym = pop_symbol();
            _symtab.insert(sym->name(), sym);
          }
          _uninitialized_vars.erase(id);

          if (is_typed(expr->type(), cdk::TYPE_STRING)) {
            int lbl;
            cdk::string_node *s = dynamic_cast<cdk::string_node *>(expr);
            _pf.RODATA();
            _pf.ALIGN();
            _pf.LABEL(mklbl(lbl = ++_lbl));
            _pf.SSTRING(s->value());
            _pf.DATA();
            _pf.ALIGN();
            if (ids->size() != 1) _pf.LABEL(id);
            _pf.SADDR(mklbl(lbl));
          }
          else if (is_typed(expr->type(), cdk::TYPE_DOUBLE)) {
            _pf.DATA();
            _pf.ALIGN();
            if (ids->size() != 1) _pf.LABEL(id);
            cdk::double_node *d = dynamic_cast<cdk::double_node *>(expr);
            if (d) {
              _pf.SDOUBLE(d->value());
            } else {
              cdk::integer_node *i = dynamic_cast<cdk::integer_node *>(expr);
              _pf.SDOUBLE(i->value());
            }
          }
          else if (is_typed(expr->type(), cdk::TYPE_POINTER)) {
            _pf.DATA();
            _pf.ALIGN();
            if (ids->size() != 1) _pf.LABEL(id);
            _pf.SINT(0); // only nullptr literal
          }
          else if (is_typed(expr->type(), cdk::TYPE_INT)) {
            cdk::integer_node *i = dynamic_cast<cdk::integer_node *>(expr);
            _pf.DATA();
            _pf.ALIGN();
            if (ids->size() != 1) _pf.LABEL(id);
            _pf.SINT(i->value());
          }
        }
      }
      _pf.TEXT();
    }
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_pointer_index_node(og::pointer_index_node* const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl + 2);

  node->index()->accept(this, lvl + 2);
  auto reference_type = cdk::reference_type_cast(node->base()->type());
  int referenced_size = reference_type->referenced()->size();
  if (referenced_size) {
    _pf.INT(referenced_size);
    _pf.MUL();
  }
  _pf.ADD();
}

void og::postfix_writer::do_tuple_index_node(og::tuple_index_node* const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->tuple()->accept(this, lvl + 2);
  auto structured_type = cdk::structured_type_cast(node->tuple()->type());
  size_t idx = node->index();
  size_t jump = 0;
  for (size_t i = 0; i < idx - 1; ++i) { // idx - 1 because tuples start at 1
    jump += structured_type->component(i)->size();
  }
  if (jump) {
    _pf.INT(jump);
    _pf.ADD();
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_tuple_node(og::tuple_node* const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->members()->size() == 1) {
    node->members()->node(0)->accept(this, lvl + 2);
  }
}
