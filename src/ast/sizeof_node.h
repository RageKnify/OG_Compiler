#ifndef __OG_AST_SIZEOF_NODE_H__
#define __OG_AST_SIZEOF_NODE_H__

#include <cdk/ast/expression_node.h>
#include "tuple_node.h"

namespace og {

  /**
   * Class for describing sizeof nodes.
   */
  class sizeof_node: public cdk::expression_node {
	og::tuple_node *_tuple;

  public:
    inline sizeof_node(int lineno, og::tuple_node *tuple) :
        cdk::expression_node(lineno), _tuple(tuple) {
    }

  public:
    inline og::tuple_node *tuple() {
      return _tuple;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_sizeof_node(this, level);
    }

  };

} // og

#endif
