#pragma once
#include "views.h"
#include <functional>
#include <string>

namespace senn {
namespace fcitx {
namespace im {

class StatefulIME {
public:
  virtual ~StatefulIME() {}

  virtual void ResetIM() = 0;

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
} // namespace fcitx
} // namespace senn
