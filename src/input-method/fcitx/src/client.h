#pragma once

#include <string>

namespace hachee {
namespace fcitx {

class Client {
public:
  Client();

  void ProcessKey(FcitxKeySym, uint32_t, uint32_t, std::string**);

private:
  std::string buffer_;
};

} // fcitx
} // hachee
