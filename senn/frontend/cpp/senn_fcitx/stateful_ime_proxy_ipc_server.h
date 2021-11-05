#pragma once
#include <string>
#include "ipc/ipc.h"
#include "ipc/request.h"

namespace senn {
namespace fcitx {

class StatefulIMEProxyIPCServerLauncher
  : public senn::ipc::ServerLauncher<StatefulIMEProxyIPCServerLauncher>,
    public senn::ipc::ConnectionFactory {
public:
  StatefulIMEProxyIPCServerLauncher(const std::string&);

  void Spawn() const;

  senn::ipc::Connection* GetConnection() const;

  senn::ipc::Connection* Create();

private:
  // The server must prevent the double startup by itself.
  const std::string server_program_path_;

  const std::string socket_path_;
};


// TODO: Define an independent requester.
class ReconnectableStatefulIMERequester
  : public senn::ipc::RequesterInterface {
public:
  ReconnectableStatefulIMERequester(StatefulIMEProxyIPCServerLauncher*);
  ~ReconnectableStatefulIMERequester();

  void Request(const std::string&, std::string*);

private:
  const StatefulIMEProxyIPCServerLauncher *launcher_;

  senn::ipc::Connection *conn_;
};

} // fcitx
} // senn
