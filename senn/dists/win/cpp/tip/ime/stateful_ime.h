#pragma once

#include "views.h"
#include <functional>

namespace senn {
namespace senn_win {
namespace ime {

enum InputMode {
  kDirect,
  kHiragana,
  kUnknown,
};

class StatefulIME {
public:
  virtual ~StatefulIME() {}

  virtual bool CanProcess(uint64_t) = 0;

  virtual bool ProcessInput(uint64_t,
                            std::function<void(const views::Editing &)>,
                            std::function<void(const views::Converting &)>,
                            std::function<void(const views::Committed &)>) = 0;

  virtual void ToggleInputMode() = 0;

  virtual InputMode GetInputMode() = 0;
};

} // namespace ime
} // namespace senn_win
} // namespace senn
