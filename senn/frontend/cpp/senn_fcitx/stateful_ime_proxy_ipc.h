#pragma once
#include <memory>
#include "ipc/ipc.h"
#include "ipc/request.h"
#include "ui.h"
#include "stateful_ime.h"
#include "stateful_ime_proxy_ipc_server.h"

namespace senn {
namespace fcitx {

class StatefulIMEProxyIPC : public StatefulIME {
public:
  StatefulIMEProxyIPC(std::unique_ptr<senn::ipc::RequesterInterface>);
  ~StatefulIMEProxyIPC();

  void ResetIM() override;

  INPUT_RETURN_VALUE ProcessInput(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>) override;

private:
  std::unique_ptr<senn::ipc::RequesterInterface> requester_;
};

} // fcitx
} // senn
