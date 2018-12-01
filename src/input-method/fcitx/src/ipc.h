#pragma once
#include <string>

namespace hachee {
namespace ipc {

class Client {
public:
  static Client* ConnectTo(const std::string&);

  void Send(const std::string&);

  void Close();

private:
  Client(const int);

  const int socket_fd_;
};

} // ipc
} // hachee
