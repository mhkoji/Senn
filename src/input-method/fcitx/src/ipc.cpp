#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <cstdlib>
#include <iostream>

#include "ipc.h"

namespace hachee {
namespace ipc {

Client::Client(const std::string& socket_name)
  : socket_name_(socket_name) {
}


void Client::Connect() {
  sockaddr_un addr;
  addr.sun_family = AF_LOCAL;
  strcpy(addr.sun_path, socket_name_.c_str());

  if ((socket_fd_ = socket(PF_LOCAL, SOCK_STREAM, 0)) < 0) {
    std::cerr << "Failed to create socket" << std::endl;
    std::exit(1);
  }

  if ((connect(socket_fd_,
               reinterpret_cast<sockaddr*>(&addr),
               SUN_LEN(&addr))) < 0) {
    std::cerr << "Failed to connect" << std::endl;
    std::exit(1);
  }
}


void Client::Send(const std::string &content) {
  write(socket_fd_, content.c_str(), content.size());
}


void Client::Close() {
  close(socket_fd_);
}

} // ipc
} // hachee
