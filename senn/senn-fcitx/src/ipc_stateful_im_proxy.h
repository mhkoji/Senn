#pragma once
#include "stateful_im_proxy.h"
#include "ipc.h"

namespace senn {
namespace fcitx {

class IPCStatefulIMProxy : public StatefulIMProxy {
public:
  ~IPCStatefulIMProxy();

  INPUT_RETURN_VALUE Input(
      FcitxKeySym, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::views::Committed*)>,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>);


  static IPCStatefulIMProxy* Create(senn::ipc::Connection*);

private:
  IPCStatefulIMProxy(senn::ipc::Connection*);

  senn::ipc::Connection *connection_;
};


} // fcitx
} // senn
