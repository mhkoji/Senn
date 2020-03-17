#pragma once
#include <memory>
#include "ipc/ipc.h"
#include "ipc/request.h"
#include "stateful_im.h"
#include "stateful_im_proxy_ipc_server.h"

namespace senn {
namespace fcitx {

class StatefulIMProxyIPC : public StatefulIM {
public:
  StatefulIMProxyIPC(std::unique_ptr<senn::ipc::RequesterInterface>);
  ~StatefulIMProxyIPC();

  INPUT_RETURN_VALUE Transit(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>);

private:
  std::unique_ptr<senn::ipc::RequesterInterface> requester_;
};

} // fcitx
} // senn
