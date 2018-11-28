#pragma once

#include <string>

namespace hachee {
namespace fcitx {

class Client {
public:
  void ProcessKey(FcitxKeySym, uint32_t, uint32_t, std::string*);
};

} // fcitx
} // hachee
