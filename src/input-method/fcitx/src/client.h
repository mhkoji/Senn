#pragma once
#include <string>

#include "ipc.h"

namespace hachee {
namespace fcitx {

class Client {
public:
  Client();

  void ProcessKey(FcitxKeySym, uint32_t, uint32_t, std::string**);

  void InvokeServerAndConnect();

private:
  std::string buffer_;

  hachee::ipc::Client *ipc_client_;
};

} // fcitx
} // hachee
