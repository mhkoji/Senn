#include <fcitx/instance.h>
#include <sstream>
#include <cassert>

#include "stateful_ime_proxy_ipc.h"
#include "stateful_ime_proxy_ipc_json.h"

namespace senn {
namespace fcitx {
namespace im {

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

std::string ProcessInputRequest(FcitxKeySym sym,
                                uint32_t keycode,
                                uint32_t state) {
  std::stringstream ss;
  ss << "{"
     << "\"op\": \"process-input\","
     << "\"args\": {"
     << "\"sym\": " << sym << ","
     << "\"keycode\": " << keycode << ","
     << "\"state\": " << state << "}"
     << "}\n";
  return ss.str();
}

std::string SelectCandidateRequest(int index) {
  std::stringstream ss;
  ss << "{"
     << "\"op\": \"select-candidate\","
     << "\"args\": {"
     << "\"index\": " << index << "}"
     << "}\n";
  return ss.str();
}

std::string ResetIMRequest() {
  std::stringstream ss;
  ss << "{"
     << "\"op\": \"reset-im\","
     << "\"args\": {}"
     << "}\n";
  return ss.str();
}

} // namespace


StatefulIMEProxyIPC::StatefulIMEProxyIPC(
    std::unique_ptr<senn::ipc::RequesterInterface> requester)
  : requester_(std::move(requester)) {
}

StatefulIMEProxyIPC::~StatefulIMEProxyIPC() {
  requester_.reset();
}

void
StatefulIMEProxyIPC::ResetIM() {
  std::string response = "";
  requester_->Request(ResetIMRequest(), &response);
  assert(response == "OK");
}

boolean
StatefulIMEProxyIPC::SelectCandidate(int index) {
  std::string response = "";
  requester_->Request(SelectCandidateRequest(index), &response);
  std::istringstream iss(response);
  boolean ok;
  iss >> ok;
  return ok;
}


INPUT_RETURN_VALUE
StatefulIMEProxyIPC::ProcessInput(
    FcitxKeySym sym, uint32_t keycode, uint32_t state,
    std::function<void(const senn::fcitx::im::views::Converting*)> on_conv,
    std::function<void(const senn::fcitx::im::views::Editing*)> on_editing) {
  std::string response = "";
  requester_->Request(ProcessInputRequest(sym, keycode, state), &response);

  std::istringstream iss(response);
  std::string input_return_value;
  boolean has_view;
  iss >> input_return_value >> has_view;

  if (has_view) {
    std::string type;
    iss >> type;
    if (type == "CONVERTING") {
      std::string content;
      std::getline(iss, content);

      senn::fcitx::im::views::Converting converting;
      senn::fcitx::im::stateful_ime_proxy_ipc_json::Parse(content,
                                                          &converting);
      on_conv(&converting);
    } else {
      std::string content;
      std::getline(iss, content);

      senn::fcitx::im::views::Editing editing;
      senn::fcitx::im::stateful_ime_proxy_ipc_json::Parse(content, &editing);
      on_editing(&editing);
    }
  }

  return ParseInputReturnValue(input_return_value);
}

} // im
} // fcitx
} // senn
