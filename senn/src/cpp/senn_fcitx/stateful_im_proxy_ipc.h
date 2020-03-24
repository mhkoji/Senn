#pragma once
#include <memory>
#include "ipc/ipc.h"
#include "ipc/request.h"
#include "input_processor.h"
#include "ui.h"
#include "stateful_im_proxy_ipc_server.h"

namespace senn {
namespace fcitx {

class StatefulIMProxyIPC
  : public InputProcessor,
    public ui::MenuHandlerInterface {
public:
  StatefulIMProxyIPC(std::unique_ptr<senn::ipc::RequesterInterface>);
  ~StatefulIMProxyIPC();

  INPUT_RETURN_VALUE ProcessInput(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>);

  boolean OnAbout();

private:
  std::unique_ptr<senn::ipc::RequesterInterface> requester_;
};

} // fcitx
} // senn
