#ifndef __OG_TARGETS_SYMBOL_H__
#define __OG_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace og {

  class symbol {
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    int _offset;
    int _qualifier;
    bool _is_function;
    bool _defined;
    bool _global;

  public:
    symbol(std::shared_ptr<cdk::basic_type> type, const std::string &name) :
        _type(type), _name(name), _offset(0), _qualifier(0),
        _is_function(false), _defined(false), _global(0) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }

    void offset(int o) {
      _offset = o;
    }

    int offset() const {
      return _offset;
    }

    void qualifier(int q) {
      _qualifier = q;
    }

    int qualifier() const {
      return _qualifier;
    }

    void is_function(bool f) {
      _is_function = f;
    }

    bool is_function() const {
      return _is_function;
    }

    void defined(bool d) {
      _defined = d;
    }

    bool defined() const {
      return _defined;
    }

    void global(bool g) {
      _global = g;
    }

    bool global() const {
      return _global;
    }
  };

} // og

#endif
