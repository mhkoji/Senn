#pragma once
#include <fcitx/instance.h>
#include <string>
#include <functional>

#include "states.h"

namespace senn {
namespace fcitx {

class Client {
public:
  virtual ~Client() {};

  virtual INPUT_RETURN_VALUE TransitByInput(
      FcitxKeySym,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Committed*)>,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Converting*)>,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Editing*)>) = 0;
};

} // fcitx
} // senn
