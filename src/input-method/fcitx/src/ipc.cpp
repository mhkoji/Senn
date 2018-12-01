#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <cstdlib>
#include <iostream>

#include "ipc.h"

namespace hachee {
namespace ipc {

Connection* Connection::ConnectTo(const std::string &socket_name) {
  sockaddr_un addr;
  addr.sun_family = AF_LOCAL;
  strcpy(addr.sun_path, socket_name.c_str());

  int socket_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
  if (socket_fd < 0) {
    std::cerr << "Failed to create socket" << std::endl;
    std::exit(1);
  }

  if ((connect(socket_fd,
               reinterpret_cast<sockaddr*>(&addr),
               SUN_LEN(&addr))) < 0) {
    std::cerr << "Failed to connect" << std::endl;
    std::exit(1);
  }

  return new Connection(socket_fd);
}


Connection::Connection(const int socket_fd)
  : socket_fd_(socket_fd) {
}


void Connection::Send(const std::string &content) {
  write(socket_fd_, content.c_str(), content.size());
}


void Connection::Close() {
  close(socket_fd_);
}

} // ipc
} // hachee
