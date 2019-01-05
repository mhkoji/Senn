#pragma once
#include "client.h"
#include "ipc.h"

namespace senn {
namespace fcitx {

class IPCClient : public Client {
public:
  ~IPCClient();

  INPUT_RETURN_VALUE DoInput(
      FcitxKeySym,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Committed*)>,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Converting*)>,
      std::function<INPUT_RETURN_VALUE(
          const senn::fcitx::states::Editing*)>);


  static IPCClient* Create(const std::string&);

private:
  IPCClient(senn::ipc::Connection*);

  senn::ipc::Connection *connection_;
};


} // fcitx
} // senn
