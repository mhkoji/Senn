#include <fcitx/instance.h>
#include <sstream>

#include "stateful_im_proxy_ipc.h"
#include "stateful_im_proxy_ipc_json.h"

namespace senn {
namespace fcitx {

namespace {

INPUT_RETURN_VALUE ParseInputReturnValue(const std::string &s) {
  if (s == "IRV_TO_PROCESS") {
    return IRV_TO_PROCESS;
  } else if (s == "IRV_DO_NOTHING") {
    return IRV_DO_NOTHING;
  } else if (s == "IRV_FLAG_FORWARD_KEY") {
    return IRV_FLAG_FORWARD_KEY;
  }
  return IRV_TO_PROCESS;
}


std::string MakeRequest(FcitxKeySym sym,
                        uint32_t keycode,
                        uint32_t state) {
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
    senn::ipc::Connection* conn,
    senn::fcitx::StatefulIMProxyIPCServerLauncher* launcher)
  : connection_(conn),
    launcher_(launcher) {
}


INPUT_RETURN_VALUE
StatefulIMProxyIPC::Transit(
    FcitxKeySym sym, uint32_t keycode, uint32_t state,
    std::function<void(const senn::fcitx::views::Converting*)> on_converting,
    std::function<void(const senn::fcitx::views::Editing*)> on_editing) {

  std::string response = "";
  {
    std::string request = MakeRequest(sym, keycode, state);
    (new senn::ipc::ReconnectableServerRequest
      <StatefulIMProxyIPCServerLauncher>(launcher_, &connection_))
      ->Execute(request, &response);
  }

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
    senn::fcitx::StatefulIMProxyIPCServerLauncher *launcher) {
  return new StatefulIMProxyIPC(launcher->GetConnection(), launcher);
}


StatefulIMProxyIPC::~StatefulIMProxyIPC() {
  if (connection_) {
    connection_->Close();
    delete connection_;
  }
}

} // fcitx
} // senn
