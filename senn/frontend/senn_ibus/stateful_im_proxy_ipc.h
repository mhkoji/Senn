#pragma once
#include "ipc/ipc.h"
#include "ipc/request.h"
#include "stateful_im.h"

namespace senn {
namespace ibus {

class StatefulIMProxyIPC : public StatefulIM {
public:
  StatefulIMProxyIPC(std::unique_ptr<senn::ipc::RequesterInterface>);
  ~StatefulIMProxyIPC();

  bool Transit(unsigned int, unsigned int, unsigned int,
               std::function<void(const senn::fcitx::views::Converting *)>,
               std::function<void(const senn::fcitx::views::Editing *)>);

private:
  std::unique_ptr<senn::ipc::RequesterInterface> requester_;
};

} // namespace ibus
} // namespace senn
