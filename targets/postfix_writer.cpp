#include <string>
#include <sstream>
#include "cdk/basic_type.h"
#include "targets/type_checker.h"
#include "targets/function_peeker.h"
#include "targets/postfix_writer.h"
#include "ast/all.h"  // all.h is automatically generated

std::string convert_name(const std::string &name) {
  if (name == "covfefe")
    return "_main";
  else if (name == "printi")
    return "_printi";
  else if (name == "println")
    return "_println";
  else if (name == "printd")
    return "_printd";
  else if (name == "prints")
    return "_prints";
  else if (name == "readi")
    return "_readi";
  else if (name == "readd")
    return "_readd";
  else
    return name;
}

void gr8::postfix_writer::do_block_node(gr8::block_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _symtab.push();
  node->declarations()->accept(this, lvl);
  node->instructions()->accept(this, lvl);
  _symtab.pop();
}

void gr8::postfix_writer::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (auto n : node->nodes()) {
    n->accept(this, lvl);
  }
}

//----------------------------------------------------------------------------------------------------------------------

void gr8::postfix_writer::do_indexation_node(gr8::indexation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->pointer()->accept(this, lvl);                        // $ ADDR
  node->position()->accept(this, lvl);                       // $ ADDR     N_OFFSET
  _pf.I32((int) node->pointer()->type()->subtype()->size()); // $ ADDR     N_OFFSET    MULTIPLIER
  _pf.MULI32();                                                // $ ADDR     BYTE_OFFSET
  _pf.CVI32TIX();
  _pf.CVIXTI64();
  _pf.ADDI64();                                                // $ NEW_ADDR
}

void gr8::postfix_writer::do_address_of_node(gr8::address_of_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
}

//----------------------------------------------------------------------------------------------------------------------

void gr8::postfix_writer::do_stop_node(gr8::stop_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.JMP(mklbl(_stop_labels[_stop_labels.size() - node->n()]));
}

void gr8::postfix_writer::do_again_node(gr8::again_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.JMP(mklbl(_again_labels[_again_labels.size() - node->n()]));
}

void gr8::postfix_writer::do_return_node(gr8::return_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->value() != nullptr) {
    node->value()->accept(this, lvl);

    if (node->value()->type()->name() == basic_type::TYPE_INT &&
        current_function()->type()->name() == basic_type::TYPE_DOUBLE) {
      _pf.CVI32TIX();
      _pf.CVIXTFX();
      _pf.CVFXTF64();
    }
    if (current_function()->type()->name() == basic_type::TYPE_INT)
      _pf.STFVALI32();

    else if (current_function()->type()->name() == basic_type::TYPE_DOUBLE)
      _pf.STFVALF64();

    else if (current_function()->type()->name() == basic_type::TYPE_POINTER ||
             current_function()->type()->name() == basic_type::TYPE_STRING)
      _pf.STFVALI64();
  }
  _pf.JMP("_END_" + convert_name(current_function()->name()));
}

//----------------------------------------------------------------------------------------------------------------------

void gr8::postfix_writer::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}

void gr8::postfix_writer::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}

//----------------------------------------------------------------------------------------------------------------------

void gr8::postfix_writer::do_integer_node(cdk::integer_node *const node, int lvl) {
  _pf.I32(node->value()); // push an integer
}

void gr8::postfix_writer::do_string_node(cdk::string_node *const node, int lvl) {
  int lbl1;

  _pf.RODATA();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl1 = ++_lbl));
  _pf.SSTRING(node->value());

  _pf.TEXT();
  _pf.ALIGN();
  _pf.ADDR(mklbl(lbl1));
}

void gr8::postfix_writer::do_double_node(cdk::double_node *const node, int lvl) {
  _pf.F64(node->value());
}

void gr8::postfix_writer::do_null_node(gr8::null_node *const node, int lvl) {
  _pf.I32(0);
}

//----------------------------------------------------------------------------------------------------------------------

void gr8::postfix_writer::do_neg_node(cdk::neg_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  if (node->type()->name() == basic_type::TYPE_INT)
    _pf.NEGI32(); // 2-complement

  else if (node->type()->name() == basic_type::TYPE_DOUBLE)
    _pf.NEGF64();
}

void gr8::postfix_writer::do_identity_node(gr8::identity_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
}

void gr8::postfix_writer::do_not_node(cdk::not_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int l_else, l_end;
  node->argument()->accept(this, lvl);
  _pf.JZI32(mklbl(l_else = ++_lbl));
  _pf.I32(0);
  _pf.JMP(mklbl(l_end = ++_lbl));
  _pf.LABEL(mklbl(l_else));
  _pf.I32(1);
  _pf.LABEL(mklbl(l_end));
}

void gr8::postfix_writer::do_objects_node(gr8::objects_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.I32((int) node->type()->subtype()->size());
  _pf.MULI32();
  _pf.CVI32TIX();
  _pf.CVIXTI64();
  _pf.ALLOC();
  _pf.SP();
}

//----------------------------------------------------------------------------------------------------------------------

void gr8::postfix_writer::do_add_node(cdk::add_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->type()->name() != basic_type::TYPE_POINTER) {
    node->left()->accept(this, lvl);
    if (node->left()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
      _pf.CVI32TIX();
      _pf.CVIXTFX();
      _pf.CVFXTF64();
    }
    node->right()->accept(this, lvl);
    if (node->right()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
      _pf.CVI32TIX();
      _pf.CVIXTFX();
      _pf.CVFXTF64();
    }

    if (node->type()->name() == basic_type::TYPE_INT)
      _pf.ADDI32();

    else if (node->type()->name() == basic_type::TYPE_DOUBLE)
      _pf.ADDF64();

  } else {

    node->left()->accept(this, lvl);
    if (node->left()->type()->name() == basic_type::TYPE_INT) {
      _pf.I32((int) node->type()->subtype()->size());
      _pf.MULI32();
      _pf.CVI32TIX();
      _pf.CVIXTI64();
    }

    node->right()->accept(this, lvl);
    if (node->right()->type()->name() == basic_type::TYPE_INT) {
      _pf.I32((int) node->type()->subtype()->size());
      _pf.MULI32();
      _pf.CVI32TIX();
      _pf.CVIXTI64();
    }

    _pf.ADDI64();
  }
}

void gr8::postfix_writer::do_sub_node(cdk::sub_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }
  node->right()->accept(this, lvl);
  if (node->right()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }

  if (node->right()->type()->name() ==
      basic_type::TYPE_POINTER) { // no need to check if left is pointer, as they both must be
    auto label = ++_lbl;

    _pf.SUBI64();
    _pf.DUP64();
    _pf.I64(0);
    _pf.LTI64();
    _pf.JZI64(mklbl(label));
    _pf.NEGI64();
    _pf.LABEL(mklbl(label));
    _pf.CVI64TIX();
    _pf.CVIXTI32();
    _pf.I32((int) node->left()->type()->subtype()->size());
    _pf.DIVI32();

  } else {

    if (node->type()->name() == basic_type::TYPE_INT)
      _pf.SUBI32();

    else if (node->type()->name() == basic_type::TYPE_DOUBLE)
      _pf.SUBF64();

    else if (node->type()->name() == basic_type::TYPE_POINTER) {
      _pf.I32((int) node->type()->subtype()->size());
      _pf.MULI32();
      _pf.CVI32TIX();
      _pf.CVIXTI64();
      _pf.SUBI32();
    }

  }
}

void gr8::postfix_writer::do_mul_node(cdk::mul_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }
  node->right()->accept(this, lvl);
  if (node->right()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }

  if (node->type()->name() == basic_type::TYPE_INT)
    _pf.MULI32();

  else if (node->type()->name() == basic_type::TYPE_DOUBLE)
    _pf.MULF64();
}

void gr8::postfix_writer::do_div_node(cdk::div_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }
  node->right()->accept(this, lvl);
  if (node->right()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }

  if (node->type()->name() == basic_type::TYPE_INT)
    _pf.DIVI32();

  else if (node->type()->name() == basic_type::TYPE_DOUBLE)
    _pf.DIVF64();
}

void gr8::postfix_writer::do_mod_node(cdk::mod_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MODI32();
}

void gr8::postfix_writer::do_lt_node(cdk::lt_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->type()->name() == basic_type::TYPE_INT &&
      node->right()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }
  node->right()->accept(this, lvl);
  if (node->right()->type()->name() == basic_type::TYPE_INT &&
      node->left()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }

  if (node->left()->type()->name() == basic_type::TYPE_DOUBLE ||
      node->right()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CMPF64();
    _pf.CVI64TIX();
    _pf.CVIXTI32();
    _pf.I32(0);
  }
  _pf.LTI32();
}

void gr8::postfix_writer::do_le_node(cdk::le_node *const node, int lvl) {
  error(node, "should not happen", true);
}

void gr8::postfix_writer::do_ge_node(cdk::ge_node *const node, int lvl) {
  error(node, "should not happen", true);
}

void gr8::postfix_writer::do_gt_node(cdk::gt_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->type()->name() == basic_type::TYPE_INT &&
      node->right()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }
  node->right()->accept(this, lvl);
  if (node->right()->type()->name() == basic_type::TYPE_INT &&
      node->left()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }

  if (node->left()->type()->name() == basic_type::TYPE_DOUBLE ||
      node->right()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CMPF64();
    _pf.CVI64TIX();
    _pf.CVIXTI32();
    _pf.I32(0);
  }
  _pf.GTI32();
}

void gr8::postfix_writer::do_ne_node(cdk::ne_node *const node, int lvl) {
  error(node, "should not happen", true);
}

void gr8::postfix_writer::do_eq_node(cdk::eq_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  if (node->left()->type()->name() == basic_type::TYPE_INT &&
      node->right()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }
  node->right()->accept(this, lvl);
  if (node->right()->type()->name() == basic_type::TYPE_INT &&
      node->left()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }

  if (node->left()->type()->name() == basic_type::TYPE_DOUBLE ||
      node->right()->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CMPF64();
    _pf.CVI64TIX();
    _pf.CVIXTI32();
    _pf.I32(0);
  }

  if (node->left()->type()->name() == basic_type::TYPE_POINTER) //then they are both pointers
    _pf.EQI64();
  else
    _pf.EQI32();
}

void gr8::postfix_writer::do_and_node(cdk::and_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int l_end = ++_lbl;

  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JZI32(mklbl(l_end));
  node->right()->accept(this, lvl);
  _pf.AND32();
  _pf.LABEL(mklbl(l_end));
}

void gr8::postfix_writer::do_or_node(cdk::or_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int l_end;

  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JNZI32(mklbl(l_end = ++_lbl));
  node->right()->accept(this, lvl);
  _pf.OR32();
  _pf.LABEL(mklbl(l_end));
}

//---------------------------------------------------------------------------

void gr8::postfix_writer::do_identifier_node(cdk::identifier_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find(node->name());

  if (symbol->global())
    _pf.ADDR(node->name());

  else
    _pf.LOCAL(symbol->offset());
}

void gr8::postfix_writer::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);

  switch (node->type()->name()) {
    case basic_type::TYPE_INT:
      _pf.LDI32();
      break;

    case basic_type::TYPE_POINTER:
    case basic_type::TYPE_STRING:
      _pf.LDI64();
      break;

    case basic_type::TYPE_DOUBLE:
      _pf.LDF64();
      break;
  }
}

void gr8::postfix_writer::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl); // determine the new value
  if (node->rvalue()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
    _pf.CVI32TIX();
    _pf.CVIXTFX();
    _pf.CVFXTF64();
  }
  node->lvalue()->accept(this, lvl); // determine the address

  switch (node->type()->name()) {
    case basic_type::TYPE_INT:
      _pf.STI32();
      break;

    case basic_type::TYPE_POINTER:
    case basic_type::TYPE_STRING:
      _pf.STI64();
      break;

    case basic_type::TYPE_DOUBLE:
      _pf.STF64();
      break;
  }
}

//---------------------------------------------------------------------------

void gr8::postfix_writer::do_function_definition_node(gr8::function_definition_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  COMPUTE_STACK_SIZE;

  auto f = _undefined_functions.find(node->name());
  if (f != _undefined_functions.end())
    _undefined_functions.erase(f);

  _pf.TEXT();
  _pf.ALIGN();

  if (node->is_public())
    _pf.GLOBAL(convert_name(node->name()), _pf.FUNC());

  _pf.LABEL(convert_name(node->name()));
  auto symbol = _symtab.find(node->name());
  _pf.ENTER(symbol->stack_size(), symbol->arguments());

  node->block()->accept(this, lvl);

  _pf.LABEL("_END_" + convert_name(node->name()));
  _pf.LEAVE();
  _pf.RET();

  if (node->name() == "covfefe") { //only include this in main function as linker wouldn't work anyway without _main
    _pf.EXTERN("readi");
    _pf.EXTERN("readd");
    _pf.EXTERN("printi");
    _pf.EXTERN("prints");
    _pf.EXTERN("printd");
    _pf.EXTERN("println");
  }

  _symtab.pop(); // match to the other weirdly placed symtab push
}

void gr8::postfix_writer::do_function_declaration_node(gr8::function_declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // just check types
}

void gr8::postfix_writer::do_function_call_node(gr8::function_call_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = _symtab.find(node->name());

  if (!symbol->defined())
    _undefined_functions.insert(node->name());

  auto actual_it = node->arguments()->nodes().rbegin();
  auto formal_it = symbol->arguments().rbegin();
  while (actual_it != node->arguments()->nodes().rend() && formal_it != symbol->arguments().rend()) {
    if (*actual_it != nullptr && *formal_it != nullptr) {
      (*actual_it)->accept(this, lvl);
      auto actual_expr = dynamic_cast<cdk::expression_node *>(*actual_it);
      if (actual_expr->type()->name() == basic_type::TYPE_INT && (*formal_it)->name() == basic_type::TYPE_DOUBLE) {
        _pf.CVI32TIX();
        _pf.CVIXTFX();
        _pf.CVFXTF64();
      }
    }

    ++actual_it;
    ++formal_it;
  }

  _pf.CALL(convert_name(node->name()), symbol->arguments());

  int trash = 0;
  for (auto it = node->arguments()->nodes().rbegin(); it != node->arguments()->nodes().rend(); ++it) {
    trash += (int) dynamic_cast<cdk::typed_node *>(*it)->type()->size();
  }
  _pf.TRASH(trash);

  switch (node->type()->name()) {
    case basic_type::TYPE_INT:
      _pf.LDFVALI32();
      break;

    case basic_type::TYPE_POINTER:
    case basic_type::TYPE_STRING:
      _pf.LDFVALI64();
      break;

    case basic_type::TYPE_DOUBLE:
      _pf.LDFVALF64();
      break;
  }
}

void gr8::postfix_writer::do_variable_declaration_node(gr8::variable_declaration_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->is_extern()) {
    _pf.EXTERN(node->name());

  } else if (node->global()) {

    switch (node->type()->name()) {
      case basic_type::TYPE_INT: {
        auto initial = dynamic_cast<cdk::literal_node<int> *>(node->initial());
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(node->name());
        _pf.SI32(initial ? initial->value() : 0);
      }
        break;

      case basic_type::TYPE_DOUBLE: {
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(node->name());
        if (auto initial = dynamic_cast<cdk::double_node *>(node->initial())) {
          _pf.SF64(initial->value());
        } else if (auto initial = dynamic_cast<cdk::integer_node *>(node->initial())) {
          _pf.SF64(initial->value());
        } else
          _pf.SF64(0);
      }
        break;

      case basic_type::TYPE_STRING: {
        auto initial = dynamic_cast<cdk::string_node *>(node->initial());
        int lbl;
        _pf.RODATA();
        _pf.ALIGN();
        _pf.LABEL(mklbl(lbl = ++_lbl));
        _pf.SSTRING(initial ? initial->value() : "");
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(node->name());
        _pf.SADDR(mklbl(lbl));
      }
        break;

      case basic_type::TYPE_POINTER:
        _pf.DATA();
        _pf.ALIGN();
        _pf.LABEL(node->name());
        _pf.SI64(0);
        break;
    }

    _pf.TEXT(); // return to the TEXT segment
  } else {
    if (node->initial() != nullptr) {
      node->initial()->accept(this, lvl);
      if (node->initial()->type()->name() == basic_type::TYPE_INT && node->type()->name() == basic_type::TYPE_DOUBLE) {
        _pf.CVI32TIX();
        _pf.CVIXTFX();
        _pf.CVFXTF64();
      }

      _pf.LOCAL(_symtab.find(node->name())->offset());
      if (node->type()->name() == basic_type::TYPE_INT || node->type()->name() == basic_type::TYPE_STRING ||
          node->type()->name() == basic_type::TYPE_POINTER)
        _pf.STI32();

      else if (node->type()->name() == basic_type::TYPE_DOUBLE)
        _pf.STF64();
    }
  }
}

//---------------------------------------------------------------------------

void gr8::postfix_writer::do_evaluation_node(gr8::evaluation_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.TRASH((int) node->argument()->type()->size());
}

void gr8::postfix_writer::do_post_node(gr8::post_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);

  switch (node->argument()->type()->name()) {
    case basic_type::TYPE_INT:
      _pf.CALL("printi", {new basic_type(4, basic_type::TYPE_INT)});
      _pf.TRASH(4);
      break;

    case basic_type::TYPE_STRING:
      _pf.CALL("prints", {new basic_type(8, basic_type::TYPE_POINTER)});
      _pf.TRASH(4);
      break;

    case basic_type::TYPE_DOUBLE:
      _pf.CALL("printd", {new basic_type(8, basic_type::TYPE_DOUBLE)});
      _pf.TRASH(8);
      break;
  }

  _pf.CALL("println"); // print a newline
}

void gr8::postfix_writer::do_tweet_node(gr8::tweet_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);

  switch (node->argument()->type()->name()) {
    case basic_type::TYPE_INT:
      _pf.CALL("printi", {new basic_type(4, basic_type::TYPE_INT)});
      _pf.TRASH(4);
      break;

    case basic_type::TYPE_STRING:
      _pf.CALL("prints", {new basic_type(8, basic_type::TYPE_POINTER)});
      _pf.TRASH(4);
      break;

    case basic_type::TYPE_DOUBLE:
      _pf.CALL("printd", {new basic_type(8, basic_type::TYPE_DOUBLE)});
      _pf.TRASH(8);
      break;
  }
}

//---------------------------------------------------------------------------

void gr8::postfix_writer::do_read_node(gr8::read_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  switch (node->type()->name()) {
    case basic_type::TYPE_INT:
      _pf.CALL("readi");
      _pf.LDFVALI32();
      break;

    case basic_type::TYPE_DOUBLE:
      _pf.CALL("readd");
      _pf.LDFVALF64();
      break;
  }
}

//---------------------------------------------------------------------------


void gr8::postfix_writer::do_sweep_node(gr8::sweep_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int l_test = ++_lbl, l_end = ++_lbl, l_incr = ++_lbl;
  _again_labels.push_back(l_incr);
  _stop_labels.push_back(l_end);

  if (node->lvalue()->type()->name() == basic_type::TYPE_INT) {
    node->from()->accept(this, lvl);
    node->lvalue()->accept(this, lvl);
    _pf.STI32();
    _pf.LABEL(mklbl(l_test));
    node->to()->accept(this, lvl);         // $ TO
    node->lvalue()->accept(this, lvl);     // $ TO  ADDR
    _pf.LDI32();                           // $ TO [ADDR]
    _pf.SUBI32();                             // $ TO-[ADDR]
    node->by()->accept(this, lvl);         // $ TO-[ADDR] BY
    _pf.MULI32();                             // $ (TO-[ADDR])*BY
    _pf.I32(0);                            // $ (TO-[ADDR])*BY 0
    _pf.JLTI32(mklbl(l_end));                 // JMP if (TO-[ADDR])*BY > 0
    node->block()->accept(this, lvl + 2);
    _pf.LABEL(mklbl(l_incr));
    node->lvalue()->accept(this, lvl);     // $  ADDR
    _pf.LDI32();                           // $ [ADDR]
    node->by()->accept(this, lvl);         // $ [ADDR] BY
    if (node->lvalue()->type()->name() == basic_type::TYPE_POINTER) {
      _pf.I32((int) node->lvalue()->type()->subtype()->size());
      _pf.MULI32();
    }
    _pf.ADDI32();                             // $ [ADDR]+BY
    node->lvalue()->accept(this, lvl);     // $ [ADDR]+BY ADDR
    _pf.STI32();                           // ADDR = [ADDR]+BY
    _pf.JMP(mklbl(l_test));
    _pf.LABEL(mklbl(l_end));

  } else if (node->lvalue()->type()->name() == basic_type::TYPE_POINTER) {
    node->from()->accept(this, lvl);
    node->lvalue()->accept(this, lvl);
    _pf.STI64();
    _pf.LABEL(mklbl(l_test));
    node->to()->accept(this, lvl);         // $ TO
    node->lvalue()->accept(this, lvl);     // $ TO  ADDR
    _pf.LDI64();                           // $ TO [ADDR]
    _pf.SUBI64();                             // $ TO-[ADDR]
    node->by()->accept(this, lvl);         // $ TO-[ADDR] BY
    _pf.CVI32TIX();
    _pf.CVIXTI64();
    _pf.MULI64();                             // $ (TO-[ADDR])*BY
    _pf.I64(0);                            // $ (TO-[ADDR])*BY 0
    _pf.JLTI64(mklbl(l_end));                 // JMP if (TO-[ADDR])*BY > 0
    node->block()->accept(this, lvl + 2);
    _pf.LABEL(mklbl(l_incr));
    node->lvalue()->accept(this, lvl);     // $  ADDR
    _pf.LDI64();                           // $ [ADDR]
    node->by()->accept(this, lvl);         // $ [ADDR] BY
    _pf.I32((int) node->lvalue()->type()->subtype()->size());
    _pf.MULI32();
    _pf.CVI32TIX();
    _pf.CVIXTI64();
    _pf.ADDI64();                             // $ [ADDR]+BY
    node->lvalue()->accept(this, lvl);     // $ [ADDR]+BY ADDR
    _pf.STI64();                           // ADDR = [ADDR]+BY
    _pf.JMP(mklbl(l_test));
    _pf.LABEL(mklbl(l_end));

  } else if (node->lvalue()->type()->name() == basic_type::TYPE_DOUBLE) {
    node->from()->accept(this, lvl);
    if (node->from()->type()->name() == basic_type::TYPE_INT) {
      _pf.CVI32TIX();
      _pf.CVIXTFX();
      _pf.CVFXTF64();
    }
    node->lvalue()->accept(this, lvl);
    _pf.STF64();
    _pf.LABEL(mklbl(l_test));
    node->to()->accept(this, lvl);         // $ TO
    if (node->to()->type()->name() == basic_type::TYPE_INT) {
      _pf.CVI32TIX();
      _pf.CVIXTFX();
      _pf.CVFXTF64();
    }
    node->lvalue()->accept(this, lvl);     // $ TO  ADDR
    _pf.LDF64();                        // $ TO [ADDR]
    _pf.SUBF64();                            // $ TO-[ADDR]
    node->by()->accept(this, lvl);         // $ TO-[ADDR] BY
    if (node->by()->type()->name() == basic_type::TYPE_INT) {
      _pf.CVI32TIX();
      _pf.CVIXTFX();
      _pf.CVFXTF64();
    }
    _pf.MULF64();                            // $ (TO-[ADDR])*BY
    _pf.F64(0);                         // $ (TO-[ADDR])*BY 0
    _pf.CMPF64();
    _pf.I32(0);
    _pf.JLTI32(mklbl(l_end));                 // JMP if (TO-[ADDR])*BY > 0
    node->block()->accept(this, lvl + 2);
    _pf.LABEL(mklbl(l_incr));
    node->lvalue()->accept(this, lvl);     // $  ADDR
    _pf.LDF64();                        // $ [ADDR]
    node->by()->accept(this, lvl);         // $ [ADDR] BY
    if (node->by()->type()->name() == basic_type::TYPE_INT) {
      _pf.CVI32TIX();
      _pf.CVIXTFX();
      _pf.CVFXTF64();
    }
    _pf.ADDF64();                            // $ [ADDR]+BY
    node->lvalue()->accept(this, lvl);     // $ [ADDR]+BY ADDR
    _pf.STF64();                        // ADDR = [ADDR]+BY
    _pf.JMP(mklbl(l_test));
    _pf.LABEL(mklbl(l_end));
  }

  _again_labels.pop_back();
  _stop_labels.pop_back();
}

//---------------------------------------------------------------------------

void gr8::postfix_writer::do_if_node(gr8::if_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int l_end;
  node->condition()->accept(this, lvl);
  _pf.JZI32(mklbl(l_end = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(l_end));
}

//---------------------------------------------------------------------------

void gr8::postfix_writer::do_if_else_node(gr8::if_else_node *const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int l_else, l_end;
  node->condition()->accept(this, lvl);
  _pf.JZI32(mklbl(l_else = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(l_end = ++_lbl));
  _pf.LABEL(mklbl(l_else));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(l_end));
}
