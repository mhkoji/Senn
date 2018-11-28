#include <fcitx/instance.h>

#include <iostream>
#include "client.h"

namespace hachee {
namespace fcitx {

void Client::ProcessKey(FcitxKeySym sym,
                        uint32_t keycode,
                        uint32_t state,
                        std::string *result_string) {
  std::cout << sym << " " << keycode << " " << state << std::endl;
}


} // fcitx
} // hachee
