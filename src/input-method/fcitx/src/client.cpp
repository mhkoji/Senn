#include <fcitx/instance.h>

#include "client.h"
#include "hachee.h"
#include "ipc.h"

namespace hachee {
namespace fcitx {

Client::Client()
  : buffer_(""),
    ipc_client_(nullptr) {}

void Client::ProcessKey(FcitxKeySym sym,
                        uint32_t keycode,
                        uint32_t state,
                        std::string **result) {
  // std::cout << sym << " " << keycode << " " << state << std::endl;
  ipc_client_->Send("test");
  buffer_ += char(sym);
  *result = &buffer_;
}

void Client::InvokeServerAndConnect() {
  const std::string socket_name = "/tmp/hachee.sock";

  hachee::InvokeIMServer(socket_name);

  ipc_client_ = new hachee::ipc::Client(socket_name);
  ipc_client_->Connect();
}

} // fcitx
} // hachee
