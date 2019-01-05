#include <string>
#include <spawn.h>
#include <unistd.h>

#include "ipc_server.h"

extern "C" {

extern char **environ;

} // extern "C"

namespace senn {
namespace fcitx {

const std::string SERVER_PROGRAM_PATH = "/usr/lib/senn/server";

bool InvokeIPCServer(const std::string &socket_name) {
  // TODO: 動かす
  // pid_t pid;
  // char *argv[] = {NULL};
  // const int status = posix_spawn(
  //     &pid, SERVER_PROGRAM_PATH.c_str(), NULL, NULL, argv, environ);

  // return status == 0;
  return true;
}

} // fcitx
} // senn
