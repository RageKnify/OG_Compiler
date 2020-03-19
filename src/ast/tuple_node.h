#ifndef __OG_AST_TUPLE_NODE_H__
#define __OG_AST_TUPLE_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace og {

	class tuple_node: public cdk::expression_node {
		cdk::sequence_node* _members;

	public:
		tuple_node(int lineno, cdk::sequence_node* members) :
			expression_node(lineno), _members(members) {
		}

	public:
		inline cdk::sequence_node* members() {
			return _members;
		}

	public:
		void accept(basic_ast_visitor* sp, int level) {
			sp->do_tuple_node(this, level);
		}
	};

}

#endif
