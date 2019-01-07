#pragma once
#include <fcitx/instance.h>
#include <string>
#include <functional>

#include "views.h"

namespace senn {
namespace fcitx {

class StatefulIMProxy {
public:
  virtual ~StatefulIMProxy() {};

  virtual INPUT_RETURN_VALUE Input(
      FcitxKeySym,
      std::function<void(const senn::fcitx::views::Committed*)>,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>) = 0;
};

} // fcitx
} // senn
