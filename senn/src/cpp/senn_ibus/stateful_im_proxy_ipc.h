#pragma once
#include "senn_fcitx/stateful_im_proxy_ipc_server.h"
#include "stateful_im.h"

namespace senn {
namespace ibus {

class StatefulIMProxyIPC : public StatefulIM {
public:
  ~StatefulIMProxyIPC();

  bool Transit(
      unsigned int, unsigned int, unsigned int,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>);

private:
  StatefulIMProxyIPC(senn::ipc::Connection*);

  senn::ipc::Connection *connection_;

public:
  static StatefulIMProxyIPC* Create(senn::ipc::Connection*);
};


} // ibus
} // senn
