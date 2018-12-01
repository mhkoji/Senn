#include <fcitx/instance.h>

#include "client.h"
#include "ipc.h"

namespace hachee {
namespace fcitx {

Client::Client()
  : buffer_(""),
    connection_(nullptr) {}

void Client::ProcessKey(FcitxKeySym sym,
                        uint32_t keycode,
                        uint32_t state,
                        std::string **result) {
  // std::cout << sym << " " << keycode << " " << state << std::endl;
  // connection_->Send("test");
  buffer_ += char(sym);
  *result = &buffer_;
}

void Client::SetConnection(hachee::ipc::Connection *conn) {
  connection_ = conn;
}

} // fcitx
} // hachee
