#pragma once
#include <unistd.h>
#include <iostream>
#include <string>

namespace senn {
namespace ipc {

class Connection {
public:
  static Connection* ConnectTo(const std::string&);
  static Connection* ConnectAbstractTo(const std::string&);

  void Write(const std::string&);

  bool ReadLine(int, std::string*);

  void Close();

private:
  Connection(const int);

  const int socket_fd_;
};


// Trying a kind of DCI implementation.
template <class ConcreteDerived>
class ServerLauncher {
public:
  Connection *ReconnectOnFailure() const {
    // Ensure that the server is started because the failure may be
    // due to an unexpected stop of the server.
    self()->Spawn();
    // Wait for the server start for a while
    usleep(kWaitIntervalForServerMsec * 1000);
    return self()->GetConnection();
  }

private:
  const ConcreteDerived* self() const {
    return static_cast<const ConcreteDerived*>(this);
  }

  static const int kWaitIntervalForServerMsec = 1000;
};


template <class ConcreteDerived>
class ReconnectableServerRequest {
public:
  ReconnectableServerRequest(
      const ServerLauncher<ConcreteDerived>* const launcher,
      Connection **server_conn)
    : launcher_(launcher),
      server_conn_(server_conn) {
  }

  void Execute(const std::string &request, std::string *response) {
    TryExecuting(0, request, response);
  }

private:
  void TryExecuting(int try_count,
                    const std::string &request,
                    std::string *response) {
    (*server_conn_)->Write(request);

    if ((*server_conn_)->ReadLine(kReadTimeoutMsec, response)) {
      return;
    }

    if (try_count < 2) {
      (*server_conn_)->Close();
      *server_conn_ = launcher_->ReconnectOnFailure();
      TryExecuting(1 + try_count, request, response);
      return;
    }

    // Because there seems to be nothing we can do, die.
    std::cerr << "Failed to request" << std::endl;
    std::exit(1);
  }

  const ServerLauncher<ConcreteDerived>* const launcher_;

  Connection **server_conn_;

  static const int kReadTimeoutMsec = 1000;
};


} // ipc
} // senn
