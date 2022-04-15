#include "user_dict.h"
#include <iostream>

// get home directory
#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>


// g++ user_dict_main.cpp user_dict.cpp -std=c++11
// g++ -shared -fPIC user_dict.cpp -o user_dict.so -std=c++11
int main(void) {
  std::string home_dir = std::string(getpwuid(getuid())->pw_dir);
  std::string user_dict_path = home_dir + "/.senn/user-dict.txt";
  user_dict_t ud = user_dict_load(user_dict_path.c_str());
  if (ud == NULL) {
    std::cout << "no such file" << std::endl;
    return 0;
  }

  std::cout << "form, pron" << std::endl;
  for (int i = 0; i < user_dict_count(ud); i++) {
    entry_t e = user_dict_entry(ud, i);
    std::cout << entry_form(e) << ", " << entry_pron(e) << std::endl;
  }
  user_dict_destroy(ud);

  return 0;
}
