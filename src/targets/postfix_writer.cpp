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
  // simplified generation: all variables are global
  _pf.ADDR(node->name());
}

void og::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  _pf.LDINT(); // depends on type size
}

void og::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl); // determine the new value
  _pf.DUP32();
  if (new_symbol() == nullptr) {
    node->lvalue()->accept(this, lvl); // where to store the value
  } else {
    _pf.DATA(); // variables are all global and live in DATA
    _pf.ALIGN(); // make sure we are aligned
    _pf.LABEL(new_symbol()->name()); // name variable location
    reset_new_symbol();
    _pf.SINT(0); // initialize it to 0 (zero)
    _pf.TEXT(); // return to the TEXT segment
    node->lvalue()->accept(this, lvl);  //DAVID: bah!
  }
  _pf.STINT(); // store the value at address
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
  _pf.CALL("readi");
  _pf.LDFVAL32();
  // node->argument()->accept(this, lvl);
  _pf.STINT();
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_for_node(og::for_node * const node, int lvl) {
#if 0
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl2 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl1));
  _pf.LABEL(mklbl(lbl2));
#endif
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
}

void og::postfix_writer::do_function_declaration_node(og::function_declaration_node *const node, int lvl) {
}

void og::postfix_writer::do_function_call_node(og::function_call_node *const node, int lvl) {
}

void og::postfix_writer::do_block_node(og::block_node *const node, int lvl) {
  /* TODO: IMPLEMENT */
  node->instructions()->accept(this, lvl+2);
}

void og::postfix_writer::do_function_definition_node(og::function_definition_node *const node, int lvl) {
  /* TODO: IMPLEMENT */
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");

  frame_size_calculator lsc(_compiler, _symtab);
  node->accept(&lsc, lvl);
  int local_size = lsc.localsize();
  if (local_size) {
    _pf.ENTER(local_size);
  } else {
    _pf.START();
  }

  _in_function = true;

  node->block()->accept(this, lvl+2);

  _in_function = false;

  _pf.LEAVE();
  _pf.RET();

  if (node->identifier() == "og") { // TODO: move somewhere else. This isn't guaranteed to capture everything
    // declare external functions
    for (std::string s: _functions_to_declare) {
      _pf.EXTERN(s);
    }

    // allocate uninitialized variables
    if (_uninitialized_vars.size() > 0) {
      _pf.BSS();
      _pf.ALIGN();
      for (std::string s : _uninitialized_vars)
      {
        auto symbol = _symtab.find(s);
        if (symbol->qualifier() == tPUBLIC)
          _pf.GLOBAL(symbol->name(), _pf.OBJ());

        _pf.LABEL(symbol->name());
        _pf.SALLOC(symbol->type()->size());
      }
    }
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_break_node(og::break_node* const node, int lvl) {
}

void og::postfix_writer::do_continue_node(og::continue_node * const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_address_of_node(og::address_of_node * const node, int lvl) {
}

void og::postfix_writer::do_nullptr_node(og::nullptr_node* const node, int lvl) {
  _pf.INT(0);
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_return_node(og::return_node* const node, int lvl) {
  /* TODO: IMPLEMENT */
  _pf.INT(0);
  _pf.STFVAL32();
  _pf.LEAVE();
  _pf.RET();
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_variable_declaration_node(og::variable_declaration_node* const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_in_function) {
    /* TODO: local variables */
  } else {
    /* TODO: must handle case in which many symbols are created */
    if (new_symbol()) {
      _uninitialized_vars.insert(new_symbol()->name());
      reset_new_symbol();
    }

    if (node->initializer()) {
      if (!node->is_auto()) {
        std::string id = *node->identifiers()->at(0);
        _uninitialized_vars.erase(id);
        if (is_typed(node->initializer()->type(), cdk::TYPE_STRING)) {
          int lbl;
          cdk::string_node *s = dynamic_cast<cdk::string_node*>(node->initializer());
          _pf.RODATA();
          _pf.ALIGN();
          _pf.LABEL(mklbl(lbl = ++_lbl));
          _pf.SSTRING(s->value());
          _pf.DATA();
          _pf.LABEL(id);
          _pf.SADDR(mklbl(lbl));
        } else if (is_typed(node->initializer()->type(), cdk::TYPE_DOUBLE)) {
          cdk::double_node *d = dynamic_cast<cdk::double_node*>(node->initializer());
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SDOUBLE(d->value());
        } else if (is_typed(node->initializer()->type(), cdk::TYPE_POINTER)) {
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SINT(0); // only nullptr literal
        } else if (is_typed(node->initializer()->type(), cdk::TYPE_INT)) {
          cdk::integer_node *i = dynamic_cast<cdk::integer_node*>(node->initializer());
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SINT(i->value());
        }

        _pf.TEXT();
      } else {
        /* TODO: tuple */
      }

      _pf.TEXT();
    }
  }
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_pointer_index_node(og::pointer_index_node* const node, int lvl) {
}

void og::postfix_writer::do_tuple_index_node(og::tuple_index_node* const node, int lvl) {
}

//---------------------------------------------------------------------------

void og::postfix_writer::do_tuple_node(og::tuple_node* const node, int lvl) {
}
