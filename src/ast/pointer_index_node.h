#ifndef __POINTER_INDEX_H__
#define __POINTER_INDEX_H__

#include <cdk/ast/lvalue_node.h>
#include "targets/basic_ast_visitor.h"

namespace og {

	class pointer_index_node: public cdk::lvalue_node {
		cdk::expression_node* _base;
		cdk::expression_node* _index;

	public:
		inline pointer_index_node(int lineno, cdk::expression_node* base, cdk::expression_node* index) :
			cdk::lvalue_node(lineno), _base(base), _index(index) {
		}

	public:
		inline cdk::expression_node* base() {
			return _base;
		}
		inline cdk::expression_node* index() {
			return _index;
		}

		void accept(basic_ast_visitor* sp, int level) {
			sp->do_pointer_index_node(this, level);
		}

	};

} // og

#endif
