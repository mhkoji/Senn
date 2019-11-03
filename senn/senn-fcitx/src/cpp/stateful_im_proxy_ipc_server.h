#pragma once
#include <string>
#include "ipc.h"

namespace senn {
namespace fcitx {

class StatefulIMProxyIPCServerLauncher {
public:
  StatefulIMProxyIPCServerLauncher();

  void Spawn() const;

  senn::ipc::Connection* GetConnection() const;

private:
  // The server must prevent the double startup by itself.
  const std::string server_program_path_;

  const std::string socket_path_;
};


} // fcitx
} // senn
