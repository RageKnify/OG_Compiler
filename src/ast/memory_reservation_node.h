#ifndef __OG_AST_MEMORY_RESERVATION_NODE_H__
#define __OG_AST_MEMORY_RESERVATION_NODE_H__

#include <cdk/ast/expression_node.h>

namespace og {

  /**
   * Class for describing memory reservation nodes.
   */
  class memory_reservation_node: public cdk::expression_node {
    cdk::expression_node *_argument;

  public:
    inline memory_reservation_node(int lineno, cdk::expression_node *argument) :
        cdk::expression_node(lineno), _argument(argument) {
    }

  public:
    inline cdk::expression_node *argument() {
      return _argument;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_memory_reservation_node(this, level);
    }

  };

} // og

#endif
