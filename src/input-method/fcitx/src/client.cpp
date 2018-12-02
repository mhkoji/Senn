#include <fcitx/instance.h>

#include <sstream>
#include <iostream>

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
  std::stringstream ss;
  ss << "{"
       << "\"op\": \"process-key\","
       << "\"args\": {" << "\"code\": " << sym << ","
                        << "\"state\": " << "\"" << buffer_ << "\"" << "}"
     << "}\n";
  connection_->Write(ss.str());

  buffer_ = "";
  connection_->ReadLine(&buffer_);
  *result = &buffer_;
}


void Client::SetConnection(hachee::ipc::Connection *conn) {
  if (connection_) {
    connection_->Close();
  }
  connection_ = conn;
}

} // fcitx
} // hachee
