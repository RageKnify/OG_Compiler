#ifndef __OG_AST_MEMORY_RESERVATION_NODE_H__
#define __OG_AST_MEMORY_RESERVATION_NODE_H__

#include <cdk/ast/unary_operation_node.h>

namespace og {

  /**
   * Class for describing memory reservation nodes.
   */
  class memory_reservation_node: public cdk::unary_operation_node {

  public:
    inline memory_reservation_node(int lineno, cdk::expression_node *argument) :
        cdk::unary_operation_node(lineno, argument) {
    }

  public:

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_memory_reservation_node(this, level);
    }

  };

} // og

#endif
