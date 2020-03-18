#ifndef __OG_AST_VARIABLE_DECLARATION_NODE_H__
#define __OG_AST_VARIABLE_DECLARATION_NODE_H__

#include <string>
#include <cdk/ast/basic_node.h>
#include <cdk/types/basic_type.h>

namespace og {

	class variable_declaration_node: public cdk::basic_node {
		int _qualifier;
		cdk::basic_type* _varType;
		std::string _identifier;
		cdk::expression_node* _initializer;

	public:
		variable_declaration_node(int lineno, int qualifier, cdk::basic_type* varType,
				const std::string &identifier, cdk::expression_node* initializer) :
			basic_node(lineno), _qualifier(qualifier), _varType(varType), _identifier(identifier),
			_initializer(initializer) {
			}
	public:
		int qualifier() {
			return _qualifier;
		}
		cdk::basic_type* varType() {
			return _varType;
		}
		const std::string& identifier() const {
			return _identifier;
		}
		cdk::expression_node* initializer() {
			return _initializer;
		}
	public:
		void accept(basic_ast_visitor* sp, int level) {
			sp->do_variable_declaration_node(this, level);
		}
	};
} // og

#endif
