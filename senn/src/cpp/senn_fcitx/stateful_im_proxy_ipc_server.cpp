#include <string>
#include <unistd.h>
#include <iostream>

#include "process/process.h"
#include "stateful_im_proxy_ipc_server.h"

namespace senn {
namespace fcitx {

StatefulIMProxyIPCServerLauncher::StatefulIMProxyIPCServerLauncher(
    const std::string &server_program_path)
  : server_program_path_(server_program_path),
    socket_path_("/tmp/senn-server-socket") {
}

void StatefulIMProxyIPCServerLauncher::Spawn() const {
  senn::process::Spawn(server_program_path_);
}

senn::ipc::Connection*
StatefulIMProxyIPCServerLauncher::GetConnection() const {
  return senn::ipc::Connection::ConnectLocalAbstractTo(socket_path_);
}

senn::ipc::Connection*
StatefulIMProxyIPCServerLauncher::Create() {
  return GetConnection();
}


ReconnectableStatefulIMRequester::ReconnectableStatefulIMRequester(
    StatefulIMProxyIPCServerLauncher *launcher)
  : launcher_(launcher), conn_(nullptr) {
}

ReconnectableStatefulIMRequester::~ReconnectableStatefulIMRequester() {
  if (conn_) {
    conn_->Close();
    delete conn_;
  }
}

void ReconnectableStatefulIMRequester::Request(const std::string &req,
                                               std::string *res) {
  if (!conn_) {
    conn_ = launcher_->GetConnection();
  }

  (new senn::ipc::ReconnectableServerRequest
    <StatefulIMProxyIPCServerLauncher>(launcher_, &conn_))
      ->Execute(req, res);
}

} // fcitx
} // senn
