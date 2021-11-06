#pragma once
#include <memory>
#include "ipc/ipc.h"
#include "ipc/request.h"
#include "stateful_ime.h"
#include "stateful_ime_proxy_ipc_server.h"
#include "views.h"

namespace senn {
namespace fcitx {
namespace im {

class StatefulIMEProxyIPC : public StatefulIME {
public:
  StatefulIMEProxyIPC(std::unique_ptr<senn::ipc::RequesterInterface>);
  ~StatefulIMEProxyIPC();

  void ResetIM() override;

  boolean SelectCandidate(int index) override;

  INPUT_RETURN_VALUE ProcessInput(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::im::views::Converting*)>,
      std::function<void(const senn::fcitx::im::views::Editing*)>) override;

private:
  std::unique_ptr<senn::ipc::RequesterInterface> requester_;
};

} // im
} // fcitx
} // senn
