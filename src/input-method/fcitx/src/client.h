#pragma once
#include <string>

#include "ipc.h"

namespace hachee {
namespace fcitx {

class Client {
public:
  Client();

  void ProcessKey(FcitxKeySym, uint32_t, uint32_t, std::string**);

  void SetConnection(hachee::ipc::Connection*);

private:
  std::string buffer_;

  hachee::ipc::Connection *connection_;
};

} // fcitx
} // hachee
