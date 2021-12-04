#pragma once
#include "fcitx/im/views.h"
#include <functional>
#include <string>

namespace senn {
namespace ibus {
namespace im {

enum InputMode {
  kDirect,
  kHiragana,
  kUnknown,
};

class StatefulIME {
public:
  virtual ~StatefulIME() {}

  virtual InputMode ToggleInputMode() = 0;

  virtual bool SelectCandidate(
      int index,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) = 0;

  virtual bool ProcessInput(
      uint32_t, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) = 0;
};

} // namespace im
} // namespace ibus
} // namespace senn
