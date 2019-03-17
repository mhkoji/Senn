#include <string>
#include <spawn.h>
#include <unistd.h>
#include <iostream>

#include "stateful_im_proxy_ipc_server.h"

extern "C" {

extern char **environ;

} // extern "C"

namespace senn {
namespace fcitx {

const std::string SERVER_PROGRAM_PATH = "/usr/lib/senn/server";

bool StartIPCServer(const std::string &socket_path) {
  pid_t pid;
  char path[SERVER_PROGRAM_PATH.size()];
  SERVER_PROGRAM_PATH.copy(path, SERVER_PROGRAM_PATH.size());
  char *argv[] = {path, NULL};
  const int status = posix_spawn(
      &pid, SERVER_PROGRAM_PATH.c_str(), NULL, NULL, argv, environ);
  return status == 0;
}

} // fcitx
} // senn
