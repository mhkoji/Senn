#pragma once
#include <string>
#include "ipc/ipc.h"
#include "ipc/request.h"

namespace senn {
namespace fcitx {

class StatefulIMProxyIPCServerLauncher
  : public senn::ipc::ServerLauncher<StatefulIMProxyIPCServerLauncher>,
    public senn::ipc::ConnectionFactory {
public:
  StatefulIMProxyIPCServerLauncher(const std::string&);

  void Spawn() const;

  senn::ipc::Connection* GetConnection() const;

  senn::ipc::Connection* Create();

private:
  // The server must prevent the double startup by itself.
  const std::string server_program_path_;

  const std::string socket_path_;
};


// TODO: Define an independent requester.
class ReconnectableStatefulIMRequester
  : public senn::ipc::RequesterInterface {
public:
  ReconnectableStatefulIMRequester(StatefulIMProxyIPCServerLauncher*);
  ~ReconnectableStatefulIMRequester();

  void Request(const std::string&, std::string*);

private:
  const StatefulIMProxyIPCServerLauncher *launcher_;

  senn::ipc::Connection *conn_;
};

} // fcitx
} // senn
