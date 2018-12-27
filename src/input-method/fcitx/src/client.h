#pragma once
#include <fcitx/instance.h>
#include <string>

#include "ipc.h"

namespace hachee {
namespace fcitx {

class Client {
public:
  Client();

  void DoInput(FcitxInstance*);

  void SetConnection(hachee::ipc::Connection*);

private:
  hachee::ipc::Connection *connection_;
};

} // fcitx
} // hachee
