#ifndef __OG_TARGET_FRAME_SIZE_CALCULATOR_H__
#define __OG_TARGET_FRAME_SIZE_CALCULATOR_H__

#include "targets/basic_ast_visitor.h"

namespace og {

	class frame_size_calculator: public basic_ast_visitor {
		size_t _localsize;
		size_t _max_tup;

	public:
		frame_size_calculator(std::shared_ptr<cdk::compiler> compiler) :
			basic_ast_visitor(compiler), _localsize(0), _max_tup(0) {
		}

		~frame_size_calculator();

		size_t localsize() const {
			return _localsize + _max_tup;
		}

	// do not edit these lines
#define __IN_VISITOR_HEADER__
#include "ast/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
	// do not edit these lines: end

	};

} // og

#endif
