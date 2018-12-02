#include <fcitx/instance.h>

#include <sstream>
#include <iostream>

#include "client.h"
#include "ipc.h"

namespace hachee {
namespace fcitx {

Client::Client()
  : connection_(nullptr) {}


void Client::DoInput(FcitxKeySym sym,
                     uint32_t keycode,
                     uint32_t state,
                     std::string *result) {
  // std::cout << sym << " " << keycode << " " << state << std::endl;
  std::stringstream ss;
  ss << "{"
       << "\"op\": \"do-input\","
       << "\"args\": {" << "\"code\": " << sym << "}"
     << "}\n";
  connection_->Write(ss.str());
  connection_->ReadLine(result);
}


void Client::SetConnection(hachee::ipc::Connection *conn) {
  if (connection_) {
    connection_->Close();
  }
  connection_ = conn;
}

} // fcitx
} // hachee
