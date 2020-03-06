#pragma once
#include "senn_fcitx/stateful_im_proxy_ipc_server.h"
#include "stateful_im.h"

namespace senn {
namespace ibus {

class StatefulIMProxyIPC : public StatefulIM {
public:
  ~StatefulIMProxyIPC();

  boolean Transit(
      guint, guint, guint,
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


} // ibus
} // senn
