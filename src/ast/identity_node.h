#ifndef __OG_AST_IDENTITY_H__
#define __OG_AST_IDENTITY_H__

#include <cdk/ast/unary_operation_node.h>
#include "targets/basic_ast_visitor.h"

namespace og {

	class identity_node: public cdk::unary_operation_node {

	public:
		identity_node(int lineno, cdk::unary_operation_node* argument) :
			cdk::unary_operation_node(lineno, argument) {
		}

	public:
		void accept(basic_ast_visitor *sp, int level) {
			sp->do_identity_node(this, level);
		}
	};

} // og

#endif
