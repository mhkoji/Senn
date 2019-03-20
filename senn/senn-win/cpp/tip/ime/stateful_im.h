#pragma once

#include <functional>
#include "views.h"

namespace senn {
namespace senn_win {
namespace ime {

class StatefulIM {
public:
  virtual ~StatefulIM() {}

  virtual void Transit(
      uint64_t keycode,
      std::function<void(const views::Editing&)>,
      std::function<void(const views::Committed&)>) = 0;
};

}  // ime
}  // senn_win
}  // senn
