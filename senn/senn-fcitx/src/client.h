#pragma once
#include <fcitx/instance.h>
#include <string>
#include <vector>
#include <functional>

#include "ipc.h"
#include "states.h"

namespace senn {
namespace fcitx {

class Client {
public:
  Client();
  ~Client();


  INPUT_RETURN_VALUE DoInput(
      FcitxKeySym,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Committed*)>,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Converting*)>,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Editing*)>);

  void SetConnection(senn::ipc::Connection*);

private:
  senn::ipc::Connection *connection_;
};

} // fcitx
} // senn
