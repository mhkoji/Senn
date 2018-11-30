#include <fcitx/instance.h>

#include <iostream>
#include "client.h"

namespace hachee {
namespace fcitx {

Client::Client()
  : buffer_("") {}

void Client::ProcessKey(FcitxKeySym sym,
                        uint32_t keycode,
                        uint32_t state,
                        std::string **result) {
  // std::cout << sym << " " << keycode << " " << state << std::endl;
  buffer_ += char(sym);
  *result = &buffer_;
}


} // fcitx
} // hachee
