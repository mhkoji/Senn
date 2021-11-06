#include <string>
#include <unistd.h>
#include <iostream>

#include "process/process.h"
#include "stateful_ime_proxy_ipc_server.h"

namespace senn {
namespace fcitx {
namespace im {
  
StatefulIMEProxyIPCServerLauncher::StatefulIMEProxyIPCServerLauncher(
    const std::string &server_program_path)
  : server_program_path_(server_program_path),
    socket_path_("/tmp/senn-server-socket") {
}

void StatefulIMEProxyIPCServerLauncher::Spawn() const {
  senn::process::Spawn(server_program_path_);
}

senn::ipc::Connection*
StatefulIMEProxyIPCServerLauncher::GetConnection() const {
  return senn::ipc::Connection::ConnectLocalAbstractTo(socket_path_);
}

senn::ipc::Connection*
StatefulIMEProxyIPCServerLauncher::Create() {
  return GetConnection();
}


ReconnectableStatefulIMERequester::ReconnectableStatefulIMERequester(
    StatefulIMEProxyIPCServerLauncher *launcher)
  : launcher_(launcher), conn_(nullptr) {
}

ReconnectableStatefulIMERequester::~ReconnectableStatefulIMERequester() {
  if (conn_) {
    conn_->Close();
    delete conn_;
  }
}

void ReconnectableStatefulIMERequester::Request(const std::string &req,
                                                std::string *res) {
  if (!conn_) {
    conn_ = launcher_->GetConnection();
  }

  (new senn::ipc::ReconnectableServerRequest
    <StatefulIMEProxyIPCServerLauncher>(launcher_, &conn_))
      ->Execute(req, res);
}

} // im
} // fcitx
} // senn
