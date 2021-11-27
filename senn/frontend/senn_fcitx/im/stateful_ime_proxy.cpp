#include "stateful_ime_proxy.h"
#include <cassert>
#include <picojson/picojson.h>
#include <sstream>

namespace senn {
namespace fcitx {
namespace im {

namespace {

void Parse(const std::string &string_content,
           senn::fcitx::im::views::Converting *output) {
  picojson::value content;
  picojson::parse(content, string_content);

  const picojson::array forms =
      content.get<picojson::object>()["forms"].get<picojson::array>();
  for (picojson::array::const_iterator it = forms.begin(); it != forms.end();
       ++it) {
    output->forms.push_back(it->get<std::string>());
  }

  output->cursor_form_index =
      content.get<picojson::object>()["cursor-form-index"].get<double>();

  const picojson::array candidates =
      content.get<picojson::object>()["cursor-form"]
          .get<picojson::object>()["candidates"]
          .get<picojson::array>();
  for (picojson::array::const_iterator it = candidates.begin();
       it != candidates.end(); ++it) {
    output->cursor_form_candidates.push_back(it->get<std::string>());
  }

  output->cursor_form_candidate_index =
      content.get<picojson::object>()["cursor-form"]
          .get<picojson::object>()["candidate-index"]
          .get<double>();
}

void Parse(const std::string &string_content,
           senn::fcitx::im::views::Editing *output) {
  picojson::value content;
  picojson::parse(content, string_content);

  output->cursor_pos =
      content.get<picojson::object>()["cursor-pos"].get<double>();
  output->input = content.get<picojson::object>()["input"].get<std::string>();
  output->committed_input =
      content.get<picojson::object>()["committed-input"].get<std::string>();

  const picojson::array predictions =
      content.get<picojson::object>()["predictions"].get<picojson::array>();
  for (picojson::array::const_iterator it = predictions.begin();
       it != predictions.end(); ++it) {
    output->predictions.push_back(it->get<std::string>());
  }

  output->prediction_index =
      content.get<picojson::object>()["prediction-index"].get<double>();
}

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
    Parse(content, &v);
    on_conv(&v);
  } else if (type == "EDITING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Editing v;
    Parse(content, &v);
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
    Parse(content, &v);
    on_conv(&v);
  } else if (type == "EDITING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Editing v;
    Parse(content, &v);
    on_editing(&v);
  }

  return consumed;
}

} // namespace im
} // namespace fcitx
} // namespace senn
