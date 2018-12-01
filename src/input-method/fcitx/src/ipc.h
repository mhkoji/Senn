#pragma once
#include <string>

namespace hachee {
namespace ipc {

class Client {
public:
  Client(const std::string&);

  void Connect();

  void Send(const std::string&);

  void Close();

private:
  int socket_fd_;

  const std::string socket_name_;
};

} // ipc
} // hachee
