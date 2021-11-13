#pragma once
#include "ipc/ipc.h"
#include "ipc/request.h"
#include "stateful_ime.h"
#include "stateful_ime_proxy_ipc_server.h"
#include "views.h"
#include <memory>

namespace senn {
namespace fcitx {
namespace im {

class StatefulIMEProxyIPC : public StatefulIME {
public:
  StatefulIMEProxyIPC(std::unique_ptr<senn::ipc::RequesterInterface>);
  ~StatefulIMEProxyIPC();

  void ResetIM() override;

  bool SelectCandidate(
      int index,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) override;

  bool ProcessInput(
      uint32_t, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) override;

private:
  std::unique_ptr<senn::ipc::RequesterInterface> requester_;
};

} // namespace im
} // namespace fcitx
} // namespace senn
