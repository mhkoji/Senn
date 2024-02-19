#include "./home.h"
#include <pwd.h>
#include <sys/types.h>
#include <unistd.h>

namespace senn {
namespace home {

void GetUserHomeDir(std::string *res) {
  struct passwd *pw = getpwuid(getuid());
  *res = pw->pw_dir;
}

} // namespace home
} // namespace senn

#include <iostream>

int main(void) {
  std::string s;
  senn::home::GetUserHomeDir(&s);
  std::cerr << s << std::endl;
  return 0;
}
