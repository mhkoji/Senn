#pragma once

#include "views.h"
#include <functional>

namespace senn {
namespace senn_win {
namespace ime {

class StatefulIM {
public:
  virtual ~StatefulIM() {}

  virtual void Transit(uint64_t keycode,
                       std::function<void(const views::Editing &)>,
                       std::function<void(const views::Converting &)>,
                       std::function<void(const views::Committed &)>) = 0;
};

} // namespace ime
} // namespace senn_win
} // namespace senn
