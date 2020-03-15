#pragma once
#include <string>
#include "ipc/ipc.h"

namespace senn {
namespace fcitx {

class StatefulIMProxyIPCServerLauncher
  : public senn::ipc::ServerLauncher<StatefulIMProxyIPCServerLauncher>,
    public senn::ipc::ConnectionFactory {
public:
  StatefulIMProxyIPCServerLauncher();

  void Spawn() const;

  senn::ipc::Connection* GetConnection() const;

  senn::ipc::Connection* Create();

private:
  // The server must prevent the double startup by itself.
  const std::string server_program_path_;

  const std::string socket_path_;
};


} // fcitx
} // senn
