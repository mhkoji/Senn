#include "process.h"

#include <spawn.h>

namespace senn {
namespace process {

extern "C" {

extern char **environ;

} // extern "C"


bool Spawn(const std::string &server_program_path) {
  pid_t pid;
  char path[server_program_path.size()+1] = {'\0'};
  server_program_path.copy(path, server_program_path.size());
  char *argv[] = {path, NULL};
  const int status = posix_spawn(
      &pid, server_program_path.c_str(), NULL, NULL, argv, environ);
  return status == 0;
}

} // process
} // senn
