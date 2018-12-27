#include <fcitx/instance.h>

#include <sstream>
#include <iostream>

#include "client.h"
#include "ipc.h"

namespace hachee {
namespace fcitx {

Client::Client()
  : connection_(nullptr) {}


void Client::DoInput(FcitxInstance *instance) {
  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxKeySym sym = (FcitxKeySym) FcitxInputStateGetKeySym(input);
  // uint32_t keycode = FcitxInputStateGetKeyCode(input);
  // uint32_t state = FcitxInputStateGetKeyState(input);
  // std::cout << sym << " " << keycode << " " << state << std::endl;

  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"do-input\","
       << "\"args\": {" << "\"code\": " << sym << "}"
       << "}\n";
    connection_->Write(ss.str());
  }

  std::string result;
  connection_->ReadLine(&result);

  std::istringstream iss(result);
  std::string msg;
  int cursor_pos;
  iss >> msg;
  iss >> cursor_pos;

  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  // 表示している文字列を削除
  FcitxMessagesSetMessageCount(
      client_preedit, 0);
  // 下線付きの文字列を表示
  FcitxMessagesAddMessageAtLast(
      client_preedit, MSG_INPUT, "%s", msg.c_str());

  // カーソルの表示
  FcitxInputStateSetClientCursorPos(input, cursor_pos);

  FcitxUIUpdateInputWindow(instance);

}


void Client::SetConnection(hachee::ipc::Connection *conn) {
  if (connection_) {
    connection_->Close();
  }
  connection_ = conn;
}

} // fcitx
} // hachee
