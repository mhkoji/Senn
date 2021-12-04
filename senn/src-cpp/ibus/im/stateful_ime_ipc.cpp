#include <iostream>
#include <string>
#include <unistd.h>

#include "process/process.h"
#include "stateful_ime_ipc.h"

namespace senn {
namespace ibus {
namespace im {

StatefulIMEIPC::IMEServerLauncher::IMEServerLauncher(
    const std::string &server_program_path)
    : server_program_path_(server_program_path),
      socket_path_("/tmp/senn-server-socket") {}

void StatefulIMEIPC::IMEServerLauncher::Spawn() const {
  senn::process::Spawn(server_program_path_);
}

senn::ipc::Connection *
StatefulIMEIPC::IMEServerLauncher::GetConnection() const {
  return senn::ipc::Connection::ConnectLocalAbstractTo(socket_path_);
}

StatefulIMEIPC::ReconnectableRequester::ReconnectableRequester(
    IMEServerLauncher *launcher, senn::ipc::Connection **conn)
    : launcher_(launcher), conn_(conn) {}

void StatefulIMEIPC::ReconnectableRequester::Request(const std::string &req,
                                                     std::string *res) {
  (new senn::ipc::ReconnectableServerRequest<IMEServerLauncher>(launcher_,
                                                                conn_))
      ->Execute(req, res);
}

StatefulIMEIPC::StatefulIMEIPC(IMEServerLauncher *launcher)
    : StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>(
          new ReconnectableRequester(launcher, &conn_))),
      launcher_(launcher), conn_(launcher->GetConnection()) {}

StatefulIMEIPC::~StatefulIMEIPC() {
  delete conn_;
  delete launcher_;
}

StatefulIMEIPC *StatefulIMEIPC::SpawnAndCreate(const std::string &path) {
  IMEServerLauncher *launcher = new IMEServerLauncher(path);
  launcher->Spawn();
  return new StatefulIMEIPC(launcher);
}

} // namespace im
} // namespace ibus
} // namespace senn
