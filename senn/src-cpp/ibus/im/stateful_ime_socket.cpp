#include <iostream>
#include <string>
#include <unistd.h>

#include "process/process.h"
#include "stateful_ime_socket.h"

namespace senn {
namespace ibus {
namespace im {

StatefulIMESocket::IMEServerLauncher::IMEServerLauncher(
    const std::string &server_program_path)
    : server_program_path_(server_program_path),
      socket_path_("/tmp/senn-server-socket") {}

void StatefulIMESocket::IMEServerLauncher::Spawn() const {
  senn::process::Spawn(server_program_path_);
}

senn::ipc::socket::Connection *
StatefulIMESocket::IMEServerLauncher::GetConnection() const {
  return senn::ipc::socket::Connection::ConnectLocalAbstractTo(socket_path_);
}

StatefulIMESocket::ReconnectableRequester::ReconnectableRequester(
    IMEServerLauncher *launcher, senn::ipc::socket::Connection *conn)
    : launcher_(launcher), conn_(conn) {}

void StatefulIMESocket::ReconnectableRequester::Request(const std::string &req,
                                                        std::string *res) {
  (new senn::ipc::socket::ReconnectableServerRequest<IMEServerLauncher>(
       launcher_, &conn_))
      ->Execute(req, res);
}

StatefulIMESocket::ReconnectableRequester::~ReconnectableRequester() {
  delete launcher_;
  delete conn_;
}

StatefulIMEProxy *StatefulIMESocket::SpawnAndCreate(const std::string &path) {
  IMEServerLauncher *launcher = new IMEServerLauncher(path);
  launcher->Spawn();
  senn::ipc::socket::Connection *conn = launcher->GetConnection();
  return new StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>(
      new StatefulIMESocket::ReconnectableRequester(launcher, conn)));
}

StatefulIMEProxy *StatefulIMESocket::ConnectTo(unsigned short port) {
  return new StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>(
      new senn::ipc::socket::Requester(
          senn::ipc::socket::Connection::ConnectTo(port))));
}

} // namespace im
} // namespace ibus
} // namespace senn
