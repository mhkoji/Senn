#pragma once
#include "ipc/socket.h"
#include "request.h"
#include "stateful_ime_proxy.h"
#include <string>

namespace senn {
namespace ibus {
namespace im {

class StatefulIMESocket : public StatefulIMEProxy {
  class IMEServerLauncher
      : public senn::ipc::socket::ServerLauncher<IMEServerLauncher> {
  public:
    IMEServerLauncher(const std::string &);
    virtual ~IMEServerLauncher() {}

    void Spawn() const;

    senn::ipc::socket::Connection *GetConnection() const;

  private:
    // The server must prevent the double startup by itself.
    const std::string server_program_path_;

    const std::string socket_path_;
  };

  class ReconnectableRequester : public senn::RequesterInterface {
  public:
    ReconnectableRequester(IMEServerLauncher *,
                           senn::ipc::socket::Connection **conn);

    void Request(const std::string &, std::string *);

  private:
    const IMEServerLauncher *launcher_;

    senn::ipc::socket::Connection **conn_;
  };

public:
  StatefulIMESocket(IMEServerLauncher *);
  ~StatefulIMESocket();

private:
  const IMEServerLauncher *launcher_;

  senn::ipc::socket::Connection *conn_;

public:
  static StatefulIMESocket *SpawnAndCreate(const std::string &path);
};

} // namespace im
} // namespace ibus
} // namespace senn
