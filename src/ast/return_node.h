#ifndef __OG_AST_RETURN_NODE_H__
#define __OG_AST_RETURN_NODE_H__

#include <cdk/ast/expression_node.h>
#include "ast/tuple_node.h"

namespace og {

	class return_node: public cdk::expression_node {
		og::tuple_node* _retval;

	public:
		return_node(int lineno, og::tuple_node* retval) :
			cdk::expression_node(lineno), _retval(retval) {
		}

	public:
		og::tuple_node* retval() {
			return _retval;
		}

	public:
		void accept(basic_ast_visitor* sp, int level) {
			sp->do_return_node(this, level);
		}
	};
}

#endif
