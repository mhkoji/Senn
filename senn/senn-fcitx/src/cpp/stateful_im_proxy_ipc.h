#pragma once
#include "stateful_im.h"
#include "stateful_im_proxy_ipc_server.h"

namespace senn {
namespace fcitx {

class StatefulIMProxyIPC : public StatefulIM {
public:
  ~StatefulIMProxyIPC();

  INPUT_RETURN_VALUE Transit(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>);

private:
  StatefulIMProxyIPC(senn::ipc::Connection*,
                     senn::fcitx::StatefulIMProxyIPCServerLauncher*);

  senn::ipc::Connection *connection_;

  const StatefulIMProxyIPCServerLauncher *launcher_;

public:
  static StatefulIMProxyIPC* Create(
      senn::fcitx::StatefulIMProxyIPCServerLauncher*);
};


} // fcitx
} // senn
