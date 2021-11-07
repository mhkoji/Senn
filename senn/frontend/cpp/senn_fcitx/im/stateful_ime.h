#pragma once
#include "views.h"
#include <fcitx/instance.h>
#include <functional>
#include <string>

namespace senn {
namespace fcitx {
namespace im {

class StatefulIME {
public:
  virtual ~StatefulIME() {}

  virtual void ResetIM() = 0;

  virtual boolean SelectCandidate(int index) = 0;

  virtual boolean ProcessInput(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) = 0;
};

} // namespace im
} // namespace fcitx
} // namespace senn
