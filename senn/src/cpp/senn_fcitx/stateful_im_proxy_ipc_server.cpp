#include <string>
#include <spawn.h>
#include <unistd.h>
#include <iostream>

#include "stateful_im_proxy_ipc_server.h"

extern "C" {

extern char **environ;

} // extern "C"

namespace senn {
namespace fcitx {

namespace {

bool SpawnIPCServerProcess(const std::string &server_program_path) {
  pid_t pid;
  char path[server_program_path.size()+1] = {'\0'};
  server_program_path.copy(path, server_program_path.size());
  char *argv[] = {path, NULL};
  const int status = posix_spawn(
      &pid, server_program_path.c_str(), NULL, NULL, argv, environ);
  return status == 0;
}

} // namespace

StatefulIMProxyIPCServerLauncher::StatefulIMProxyIPCServerLauncher(
    const std::string &server_program_path)
  : server_program_path_(server_program_path),
    socket_path_("/tmp/senn-server-socket") {
}

void StatefulIMProxyIPCServerLauncher::Spawn() const {
  SpawnIPCServerProcess(server_program_path_);
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
