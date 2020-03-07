#include <sys/socket.h>
#include <sys/un.h>
#include <cstdlib>

#include <iostream>
#include <algorithm>

#include "ipc.h"

namespace senn {
namespace ipc {

namespace {

int MakeLocalStreamSocketOrDie(void) {
  int socket_fd = socket(PF_LOCAL, SOCK_STREAM, 0);
  if (socket_fd < 0) {
    std::cerr << "Failed to create socket: " << socket_fd << std::endl;
    std::exit(1);
  }

  return socket_fd;
}

bool IsReadTimeout(int socket, int timeout_msec) {
  fd_set fds;
  struct timeval tv;
  FD_ZERO(&fds);
  FD_SET(socket, &fds);
  tv.tv_sec = timeout_msec / 1000;
  tv.tv_usec = 1000 * (timeout_msec % 1000);
  if (select(socket + 1, &fds, NULL, NULL, &tv) < 0) {
    return true;
  }
  if (FD_ISSET(socket, &fds)) {
    return false;
  }
  return true;
}

} // namespace

Connection* Connection::ConnectTo(const std::string &path) {
  int socket_fd = MakeLocalStreamSocketOrDie();

  sockaddr_un addr;
  addr.sun_family = AF_LOCAL;
  strcpy(addr.sun_path, path.c_str());
  int connect_return_value = connect(
      socket_fd,
      reinterpret_cast<const struct sockaddr*>(&addr),
      sizeof(addr));
  if (connect_return_value < 0) {
    close(socket_fd);
    std::cerr << "ConnectTo: Failed to connect: "
              << connect_return_value << std::endl;
    std::exit(1);
  }

  return new Connection(socket_fd);
}

Connection* Connection::ConnectAbstractTo(const std::string &path) {
  int socket_fd = MakeLocalStreamSocketOrDie();

  sockaddr_un addr;
  addr.sun_family = AF_LOCAL;
  addr.sun_path[0] = '\0';
  path.copy(&addr.sun_path[1], path.size());
  int connect_return_value = connect(
      socket_fd,
      reinterpret_cast<const struct sockaddr*>(&addr),
      sizeof(addr) - sizeof(addr.sun_path) + path.size() + 1);
  if (connect_return_value < 0) {
    close(socket_fd);
    std::cerr << "ConnectAbstractTo: Failed to connect: "
              << connect_return_value << std::endl;
    std::exit(1);
  }

  return new Connection(socket_fd);
}


Connection::Connection(const int socket_fd)
  : socket_fd_(socket_fd) {
}


void Connection::Write(const std::string &content) {
  write(socket_fd_, content.c_str(), content.size());
}


bool Connection::ReadLine(int timeout_msec, std::string *output) {
  char buffer[1024];

  while (1) {
    if (IsReadTimeout(socket_fd_, timeout_msec)) {
      return false;
    }

    int bytes_read = read(socket_fd_, buffer, sizeof(buffer));

    if (bytes_read == -1) {
      std::cerr << "Failed to read" << std::endl;
      std::exit(1);
    }

    if (bytes_read == 0) {
      return false;
    }

    *output += std::string(buffer, size_t(bytes_read));

    if (buffer[bytes_read - 1] == '\n') {
      output->erase(std::find_if(
                        output->rbegin(),
                        output->rend(),
                        [](int ch) { return !std::isspace(ch); }
                    ).base(),
                    output->end());
      return true;
    } else if (bytes_read == sizeof(buffer)) {
      continue;
    }
    // Should not reach here
  }
}


void Connection::Close() {
  close(socket_fd_);
}


} // ipc
} // senn
