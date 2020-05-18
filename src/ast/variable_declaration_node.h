#ifndef __OG_AST_VARIABLE_DECLARATION_NODE_H__
#define __OG_AST_VARIABLE_DECLARATION_NODE_H__

#include <string>
#include <vector>
#include <cdk/ast/basic_node.h>
#include <cdk/types/basic_type.h>

namespace og {

	class variable_declaration_node: public cdk::basic_node {
		typedef std::vector<std::string*> identifiers_type;

		int _qualifier;
		std::shared_ptr<cdk::basic_type> _varType;
		identifiers_type* _identifiers;
		cdk::expression_node* _initializer;

	public:
		variable_declaration_node(int lineno, int qualifier, cdk::basic_type* varType,
				identifiers_type* identifiers, cdk::expression_node* initializer) :
			basic_node(lineno), _qualifier(qualifier), _varType(varType), _identifiers(identifiers),
			_initializer(initializer) {
			}
	public:
		int qualifier() {
			return _qualifier;
		}
		std::shared_ptr<cdk::basic_type> varType() {
			return _varType;
		}
		identifiers_type* identifiers() {
			return _identifiers;
		}
		cdk::expression_node* initializer() {
			return _initializer;
		}
		bool is_auto() {
			return _varType->name() == cdk::TYPE_UNSPEC;
		}
	public:
		void accept(basic_ast_visitor* sp, int level) {
			sp->do_variable_declaration_node(this, level);
		}
	};
} // og

#endif
