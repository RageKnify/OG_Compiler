#ifndef __OG_AST_RETURN_NODE_H__
#define __OG_AST_RETURN_NODE_H__

#include <cdk/ast/expression_node.h>

namespace og {

	class return_node: public cdk::expression_node {
		cdk::expression_node* _retval;

	public:
		return_node(int lineno, cdk::expression_node* retval) :
			cdk::expression_node(lineno), _retval(retval) {
		}

	public:
		cdk::expression_node* retval() {
			return _retval;
		}

	public:
		void accept(basic_ast_visitor* sp, int level) {
			sp->do_return_node(this, level);
		}
	};
}

#endif
