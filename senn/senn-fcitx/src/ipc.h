#pragma once
#include <string>

namespace senn {
namespace ipc {

class Connection {
public:
  static Connection* ConnectTo(const std::string&);
  static Connection* ConnectAbstractTo(const std::string&);

  void Write(const std::string&);

  void ReadLine(std::string*);

  void Close();

private:
  Connection(const int);

  const int socket_fd_;
};

} // ipc
} // senn
