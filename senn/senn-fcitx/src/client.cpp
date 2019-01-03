#include <fcitx/instance.h>
#include <sstream>

#include "client.h"
#include "ipc.h"

namespace senn {
namespace fcitx {

Client::Client()
  : connection_(nullptr) {}


INPUT_RETURN_VALUE
Client::DoInput(FcitxKeySym code,
                std::function<
                    INPUT_RETURN_VALUE(const std::string&, const int)
                > on_committed,
                std::function<
                    INPUT_RETURN_VALUE(const std::vector<std::string>&,
                                       const int)
                > on_converting,
                std::function<
                    INPUT_RETURN_VALUE(const boolean consumed,
                                       const std::string&,
                                       const int)
                > on_editing) {
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

  std::string type;
  std::istringstream iss(result);
  iss >> type;

  if (type == "COMMITTED") {
    int cursor_pos;
    std::string input;
    iss >> cursor_pos;
    iss >> input;
    return on_committed(input, cursor_pos);
  }

  if (type == "CONVERTING") {
    int cursor_pos;
    iss >> cursor_pos;

    std::vector<std::string> forms;
    std::string form;
    while (iss >> form) {
      forms.push_back(form);
    }
    return on_converting(forms, cursor_pos);
  }

  int consumed, cursor_pos;
  std::string input;
  iss >> consumed;
  iss >> cursor_pos;
  iss >> input;
  return on_editing(consumed == 1, input, cursor_pos);
}


void Client::SetConnection(senn::ipc::Connection *conn) {
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
} // senn
