#ifndef __OG_AST_FUNCTION_DECLARATION_NODE_H__
#define __OG_AST_FUNCTION_DECLARATION_NODE_H__

#include <string>
#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/types/basic_type.h>

namespace og {

  /**
   * Class for describing function declarations nodes.
   */
  class function_declaration_node: public cdk::basic_node {
    int _qualifier;
    cdk::basic_type *_type;
    std::string _identifier;
    cdk::sequence_node *_arguments;

  public:
    inline function_declaration_node(int lineno, int qualifier, cdk::basic_type *type, std::string &identifier) :
      cdk::basic_node(lineno), _qualifier(qualifier), _type(type), _identifier(identifier), _arguments(new cdk::sequence_node(lineno)) {
    }

    inline function_declaration_node(int lineno, int qualifier, cdk::basic_type *type, std::string &identifier, cdk::sequence_node *arguments) :
      cdk::basic_node(lineno), _qualifier(qualifier), _type(type), _identifier(identifier), _arguments(arguments) {
    }

  public:
    inline int qualifier() {
      return _qualifier;
    }

    inline cdk::basic_type *type() {
      return _type;
    }

    inline std::string &identifier() {
      return _identifier;
    }

    inline cdk::sequence_node *arguments() {
      return _arguments;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_declaration_node(this, level);
    }

  };

} // og

#endif
