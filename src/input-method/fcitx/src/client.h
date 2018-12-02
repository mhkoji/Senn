#pragma once
#include <string>

#include "ipc.h"

namespace hachee {
namespace fcitx {

class Client {
public:
  Client();

  void DoInput(FcitxKeySym, uint32_t, uint32_t, std::string*);

  void SetConnection(hachee::ipc::Connection*);

private:
  hachee::ipc::Connection *connection_;
};

} // fcitx
} // hachee
