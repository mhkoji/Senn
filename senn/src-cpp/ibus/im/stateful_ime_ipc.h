#pragma once
#include "ipc/ipc.h"
#include "request.h"
#include "stateful_ime_proxy.h"
#include <string>

namespace senn {
namespace ibus {
namespace im {

class StatefulIMEIPC : public StatefulIMEProxy {
  class IMEServerLauncher
      : public senn::ipc::ServerLauncher<IMEServerLauncher> {
  public:
    IMEServerLauncher(const std::string &);
    virtual ~IMEServerLauncher() {}

    void Spawn() const;

    senn::ipc::Connection *GetConnection() const;

  private:
    // The server must prevent the double startup by itself.
    const std::string server_program_path_;

    const std::string socket_path_;
  };

  class ReconnectableRequester : public senn::RequesterInterface {
  public:
    ReconnectableRequester(IMEServerLauncher *, senn::ipc::Connection **conn);

    void Request(const std::string &, std::string *);

  private:
    const IMEServerLauncher *launcher_;

    senn::ipc::Connection **conn_;
  };

public:
  StatefulIMEIPC(IMEServerLauncher *);
  ~StatefulIMEIPC();

private:
  const IMEServerLauncher *launcher_;

  senn::ipc::Connection *conn_;

public:
  static StatefulIMEIPC *SpawnAndCreate(const std::string &path);
};

} // namespace im
} // namespace ibus
} // namespace senn
