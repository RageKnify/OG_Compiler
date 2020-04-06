#ifndef __OG_AST_NULLPTR_H
#define __OG_AST_NULLPTR_H

#include <cdk/ast/literal_node.h>
#include "targets/basic_ast_visitor.h"

namespace og {

	class nullptr_node: public cdk::literal_node<void*> {

	public:
		nullptr_node(int lineno) : literal_node<void*>(lineno, NULL) {
		}

	public:
		void accept(basic_ast_visitor* av, int level) {
			av->do_nullptr_node(this, level);
		}

	};

} // og

#endif
