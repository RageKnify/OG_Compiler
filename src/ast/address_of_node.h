#ifndef __OG_AST_ADDRESS_OF_H__
#define __OG_AST_ADDRESS_OF_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "targets/basic_ast_visitor.h"

namespace og {

	class address_of_node: public cdk::expression_node {
		cdk::lvalue_node* _lvalue;

	public:
		address_of_node(int lineno, cdk::lvalue_node* argument) :
			cdk::expression_node(lineno), _lvalue(argument) {
		}

	public:
		inline cdk::lvalue_node* lvalue() {
			return _lvalue;
		}
		void accept(basic_ast_visitor *sp, int level) {
			sp->do_address_of_node(this, level);
		}
	};

} // og

#endif
