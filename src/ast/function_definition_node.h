#ifndef __OG_AST_FUNCTION_DEFINITION_NODE_H__
#define __OG_AST_FUNCTION_DEFINITION_NODE_H__

#include <string>
#include <cdk/ast/typed_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/types/basic_type.h>
#include "ast/block_node.h"

namespace og {

  /**
   * Class for describing function definitions nodes.
   */
  class function_definition_node: public cdk::typed_node {
    int _qualifier;
    std::string _identifier;
    cdk::sequence_node *_parameters;
    og::block_node *_block;

  public:
    inline function_definition_node(int lineno, int qualifier, cdk::basic_type *type, const std::string &identifier, og::block_node *block) :
      cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _parameters(NULL), _block(block) {
        typed_node::type((std::shared_ptr<cdk::basic_type>)type);
    }

    inline function_definition_node(int lineno, int qualifier, cdk::basic_type *type, const std::string &identifier, cdk::sequence_node *parameters, og::block_node *block) :
      cdk::typed_node(lineno), _qualifier(qualifier), _identifier(identifier), _parameters(parameters), _block(block) {
        typed_node::type((std::shared_ptr<cdk::basic_type>)type);
    }

  public:
    inline int qualifier() {
      return _qualifier;
    }

    const inline std::string &identifier() const {
      return _identifier;
    }

    inline cdk::sequence_node *parameters() {
      return _parameters;
    }

    inline og::block_node *block() {
      return _block;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_definition_node(this, level);
    }

  };

} // og

#endif
