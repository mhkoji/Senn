#pragma once
#include "stateful_im.h"
#include "ipc.h"

namespace senn {
namespace fcitx {

class StatefulIMProxyIPC : public StatefulIM {
public:
  ~StatefulIMProxyIPC();

  INPUT_RETURN_VALUE Input(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::views::Committed*)>,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>);


  static StatefulIMProxyIPC* Create(senn::ipc::Connection*);

private:
  StatefulIMProxyIPC(senn::ipc::Connection*);

  senn::ipc::Connection *connection_;
};


} // fcitx
} // senn
