#include <fcitx/instance.h>

#include <sstream>
#include <iostream>

#include "client.h"
#include "ipc.h"

namespace hachee {
namespace fcitx {

Client::Client()
  : connection_(nullptr) {}


void Client::DoInput(FcitxKeySym code,
                     std::string *msg,
                     int *cursor_pos) {
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"do-input\","
       << "\"args\": {" << "\"code\": " << code << "}"
       << "}\n";
    connection_->Write(ss.str());
  }

  {
    std::string result;
    connection_->ReadLine(&result);

    std::istringstream iss(result);
    iss >> *msg;
    iss >> *cursor_pos;
  }
}


void Client::SetConnection(hachee::ipc::Connection *conn) {
  if (connection_) {
    connection_->Close();
  }
  connection_ = conn;
}

} // fcitx
} // hachee
