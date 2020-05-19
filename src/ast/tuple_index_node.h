#ifndef __OG_AST_TUPLE_INDEX_NODE_H__
#define __OG_AST_TUPLE_INDEX_NODE_H__

#include <cdk/ast/lvalue_node.h>

namespace og {

	class tuple_index_node: public cdk::lvalue_node {
		cdk::lvalue_node* _tuple;
		int _index;

	public:
		inline tuple_index_node(int lineno, cdk::lvalue_node* tuple, int index) :
			cdk::lvalue_node(lineno), _tuple(tuple), _index(index) {
		}

	public:
		inline cdk::lvalue_node* tuple() {
			return _tuple;
		}

		inline int index() {
			return _index;
		}

	public:
		void accept(basic_ast_visitor* sp, int level) {
			sp->do_tuple_index_node(this, level);
		}

	};

} // og

#endif
