#pragma once
#include <fcitx/instance.h>
#include <string>
#include <functional>
#include "views.h"

namespace senn {
namespace fcitx {
namespace im {

class StatefulIME {
public:
  virtual ~StatefulIME() {}

  virtual void ResetIM() = 0;

  virtual boolean SelectCandidate(int index) = 0;

  virtual INPUT_RETURN_VALUE ProcessInput(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::im::views::Converting*)>,
      std::function<void(const senn::fcitx::im::views::Editing*)>) = 0;
};

} // im
} // fcitx
} // senn
