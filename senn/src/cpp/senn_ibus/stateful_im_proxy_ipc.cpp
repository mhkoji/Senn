#include <sstream>
#include <iostream>

#include "stateful_im_proxy_ipc.h"
#include "senn_fcitx/stateful_im_proxy_ipc_json.h"

namespace senn {
namespace ibus {

namespace {

bool ParseInputReturnValue(const std::string &s) {
  if (s == "IRV_TO_PROCESS") {
    return false;
  } else if (s == "IRV_DO_NOTHING") {
    return true;
  } else if (s == "IRV_FLAG_FORWARD_KEY") {
    return false;
  }
  return false;
}

std::string MakeRequest(unsigned int sym,
                        unsigned int keycode,
                        unsigned int state) {
  std::stringstream ss;
  ss << "{"
     << "\"op\": \"transit\","
     << "\"args\": {" << "\"sym\": " << sym << ","
     << "\"keycode\": " << keycode << ","
     << "\"state\": " << state << "}"
     << "}\n";
  return ss.str();
}

} // namespace


StatefulIMProxyIPC::StatefulIMProxyIPC(
    senn::ipc::Connection* conn)
  : connection_(conn)  {
}

bool
StatefulIMProxyIPC::Transit(
    unsigned int sym, unsigned int  keycode, unsigned int state,
    std::function<void(const senn::fcitx::views::Converting*)> on_converting,
    std::function<void(const senn::fcitx::views::Editing*)> on_editing) {
  std::string response = "";
  requester_->Request(MakeRequest(sym, keycode, state), &response);

  std::istringstream iss(response);
  std::string input_return_value, type;
  iss >> input_return_value;
  iss >> type;

  if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::views::Converting converting;
    senn::fcitx::stateful_im_proxy_ipc_json::Parse(content, &converting);
    on_converting(&converting);
  } else {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::views::Editing editing;
    senn::fcitx::stateful_im_proxy_ipc_json::Parse(content, &editing);
    on_editing(&editing);
  }

  return ParseInputReturnValue(input_return_value);
}

StatefulIMProxyIPC* StatefulIMProxyIPC::Create(
    senn::ipc::Connection* conn) {
  return new StatefulIMProxyIPC(conn);
}


StatefulIMProxyIPC::~StatefulIMProxyIPC() {
  if (connection_) {
    connection_->Close();
    delete connection_;
  }
}

} // ibus
} // senn
