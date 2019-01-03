#pragma once
#include <fcitx/instance.h>
#include <string>
#include <vector>
#include <functional>

#include "ipc.h"

namespace senn {
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
          INPUT_RETURN_VALUE(const std::vector<std::string>&, const int)
      >,
      std::function<
          INPUT_RETURN_VALUE(const boolean consumed,
                             const std::string&,
                             const int)
      >);

  void SetConnection(senn::ipc::Connection*);

private:
  senn::ipc::Connection *connection_;
};

} // fcitx
} // senn
