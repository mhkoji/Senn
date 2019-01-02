#pragma once
#include <fcitx/instance.h>
#include <string>
#include <functional>

#include "ipc.h"

namespace hachee {
namespace fcitx {

class Client {
public:
  Client();
  ~Client();


  INPUT_RETURN_VALUE DoInput(
      FcitxKeySym,
      std::function<
          INPUT_RETURN_VALUE(const std::string&, const int)
      >,
      std::function<
          INPUT_RETURN_VALUE(const std::string&, const int)
      >);

  void SetConnection(hachee::ipc::Connection*);

private:
  hachee::ipc::Connection *connection_;
};

} // fcitx
} // hachee
