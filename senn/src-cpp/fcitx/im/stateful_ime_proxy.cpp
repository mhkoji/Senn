#include "stateful_ime_proxy.h"
#include "views_json.h"
#include <cassert>
#include <sstream>

namespace senn {
namespace fcitx {
namespace im {

namespace {

std::string ProcessInputRequest(uint32_t sym, uint32_t keycode,
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

StatefulIMEProxy::StatefulIMEProxy(
    std::unique_ptr<senn::RequesterInterface> requester)
    : requester_(std::move(requester)) {}

StatefulIMEProxy::~StatefulIMEProxy() { requester_.reset(); }

void StatefulIMEProxy::ResetIM() {
  std::string response = "";
  requester_->Request(ResetIMRequest(), &response);
  assert(response == "OK");
}

bool StatefulIMEProxy::SelectCandidate(
    int index,
    std::function<void(const senn::fcitx::im::views::Converting *)> on_conv,
    std::function<void(const senn::fcitx::im::views::Editing *)> on_editing) {
  std::string response = "";
  requester_->Request(SelectCandidateRequest(index), &response);

  std::istringstream iss(response);
  bool consumed;
  std::string type;
  iss >> consumed >> type;

  if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Converting v;
    senn::fcitx::im::views::json::Parse(content, &v);
    on_conv(&v);
  } else if (type == "EDITING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Editing v;
    senn::fcitx::im::views::json::Parse(content, &v);
    on_editing(&v);
  }

  return consumed;
}

bool StatefulIMEProxy::ProcessInput(
    uint32_t sym, uint32_t keycode, uint32_t state,
    std::function<void(const senn::fcitx::im::views::Converting *)> on_conv,
    std::function<void(const senn::fcitx::im::views::Editing *)> on_editing) {
  std::string response = "";
  requester_->Request(ProcessInputRequest(sym, keycode, state), &response);

  std::istringstream iss(response);
  bool consumed;
  std::string type;
  iss >> consumed >> type;

  if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Converting v;
    senn::fcitx::im::views::json::Parse(content, &v);
    on_conv(&v);
  } else if (type == "EDITING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Editing v;
    senn::fcitx::im::views::json::Parse(content, &v);
    on_editing(&v);
  }

  return consumed;
}

} // namespace im
} // namespace fcitx
} // namespace senn
