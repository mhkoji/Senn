#include <fcitx/instance.h>

#include <sstream>

#include "client.h"
#include "ipc.h"

namespace hachee {
namespace fcitx {

Client::Client()
  : connection_(nullptr) {}


INPUT_RETURN_VALUE
Client::DoInput(FcitxKeySym code,
                std::function<
                    INPUT_RETURN_VALUE(const std::string&, const int)
                > on_committed,
                std::function<
                    INPUT_RETURN_VALUE(const std::string&, const int)
                > on_updated) {
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"do-input\","
       << "\"args\": {" << "\"code\": " << code << "}"
       << "}\n";
    connection_->Write(ss.str());
  }

  std::string result;
  connection_->ReadLine(&result);

  std::string type, input;
  int cursor_pos;
  std::istringstream iss(result);
  iss >> type;
  iss >> cursor_pos;
  iss >> input;

  if (type == "COMMITTED") {
    return on_committed(input, cursor_pos);
  }
  return on_updated(input, cursor_pos);
}


void Client::SetConnection(hachee::ipc::Connection *conn) {
  if (connection_) {
    connection_->Close();
  }
  connection_ = conn;
}


Client::~Client() {
  if (connection_) {
    connection_->Close();
  }
}

} // fcitx
} // hachee
