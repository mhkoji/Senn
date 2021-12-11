#include "stateful_ime_exec.h"
#include "stateful_ime_proxy.h"
#include <algorithm>
#include <cassert>
#include <cstring>
#include <iostream>
#include <signal.h>
#include <unistd.h>

namespace {

bool ReadLine(int fd, std::string *output) {
  char buffer[1024];

  while (1) {
    int bytes_read = read(fd, buffer, sizeof(buffer));

    if (bytes_read == -1) {
      std::cerr << "Failed to read" << std::endl;
      std::exit(1);
    }

    if (bytes_read == 0) {
      return false;
    }

    *output += std::string(buffer, size_t(bytes_read));

    if (buffer[bytes_read - 1] == '\n') {
      output->erase(std::find_if(output->rbegin(), output->rend(),
                                 [](int ch) { return !std::isspace(ch); })
                        .base(),
                    output->end());
      return true;
    } else if (bytes_read == sizeof(buffer)) {
      continue;
    }
    // Should not reach here
  }
}

void Write(int fd, const std::string &content) {
  write(fd, content.c_str(), content.size());
}

} // namespace

namespace senn {
namespace ibus {
namespace im {

StatefulIMEExec::Requester::Requester(pid_t pid, int fd_read, int fd_write)
    : pid_(pid), fd_read_(fd_read), fd_write_(fd_write) {}

StatefulIMEExec::Requester::~Requester() {

  close(fd_read_);
  close(fd_write_);
  kill(pid_, SIGKILL);
}

void StatefulIMEExec::Requester::Request(const std::string &req,
                                         std::string *res) {
  // std::cout << req << std::endl;
  Write(fd_write_, req);
  assert(ReadLine(fd_read_, res));
}

StatefulIME *StatefulIMEExec::Create() {
  int p2c[2] = {0, 0};
  int c2p[2] = {0, 0};

  if ((pipe(p2c) < 0) || pipe(c2p)) {
    std::cerr << "Failed to pipe" << std::endl;
    std::exit(1);
  }

  pid_t pid = fork();
  if (pid == -1) {
    std::cerr << "Failed to fork" << std::endl;
    std::exit(1);
  }

  if (pid == 0) {
    // child process
    dup2(p2c[0], STDIN_FILENO);
    dup2(c2p[1], STDOUT_FILENO);
    close(p2c[0]);
    close(c2p[1]);
    char prog[] = "senn-ibus";
    char *args[2];
    args[0] = prog;
    args[1] = NULL;
    execv("/usr/lib/senn/senn-ibus", args); // no return
  }

  return new StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>(
      new StatefulIMEExec::Requester(pid, c2p[0], p2c[1])));
}

} // namespace im
} // namespace ibus
} // namespace senn
