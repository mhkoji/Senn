#include <fcitx/instance.h>
#include <sstream>
#include <picojson/picojson.h>

#include "stateful_im_proxy_ipc.h"

namespace senn {
namespace fcitx {

namespace {

void ParseConverting(picojson::value &content,
                     senn::fcitx::views::Converting *output) {
  const picojson::array forms = content
    .get<picojson::object>()["forms"]
    .get<picojson::array>();
  for (picojson::array::const_iterator it = forms.begin();
       it != forms.end(); ++it) {
    output->forms.push_back(it->get<std::string>());
  }

  output->cursor_form_index = content
    .get<picojson::object>()["cursor-form-index"]
    .get<double>();

  const picojson::array candidates = content
    .get<picojson::object>()["cursor-form"]
    .get<picojson::object>()["candidates"]
    .get<picojson::array>();
  for (picojson::array::const_iterator it = candidates.begin();
       it != candidates.end(); ++it) {
    output->cursor_form_candidates.push_back(it->get<std::string>());
  }

  output->cursor_form_candidate_index = content
    .get<picojson::object>()["cursor-form"]
    .get<picojson::object>()["candidate-index"]
    .get<double>();
}

void ParseEditing(picojson::value &content,
                  senn::fcitx::views::Editing *output) {
  output->cursor_pos = content
    .get<picojson::object>()["cursor-pos"].get<double>();
  output->input = content
    .get<picojson::object>()["input"].get<std::string>();
  output->committed_input = content
    .get<picojson::object>()["committed-input"].get<std::string>();
}

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

} // namespace


StatefulIMProxyIPC::StatefulIMProxyIPC(senn::ipc::Connection* conn)
  : connection_(conn) {}


INPUT_RETURN_VALUE
StatefulIMProxyIPC::Transit(
    FcitxKeySym sym, uint32_t keycode, uint32_t state,
    std::function<void(const senn::fcitx::views::Converting*)> on_converting,
    std::function<void(const senn::fcitx::views::Editing*)> on_editing) {

  std::string result = "";
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"transit\","
       << "\"args\": {" << "\"sym\": " << sym << ","
                        << "\"keycode\": " << keycode << ","
                        << "\"state\": " << state << "}"
       << "}\n";
    connection_->Write(ss.str());
    connection_->ReadLine(&result);
    // std::cout << result << std::endl;
  }

  std::string input_return_value, type;
  std::istringstream iss(result);
  iss >> input_return_value;
  iss >> type;

  if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    picojson::value v;
    picojson::parse(v, content);

    senn::fcitx::views::Converting converting;
    ParseConverting(v, &converting);
    on_converting(&converting);
  } else {
    std::string content;
    std::getline(iss, content);

    picojson::value v;
    picojson::parse(v, content);

    senn::fcitx::views::Editing editing;
    ParseEditing(v, &editing);
    on_editing(&editing);
  }

  return ParseInputReturnValue(input_return_value);
}

StatefulIMProxyIPC*
StatefulIMProxyIPC::Create(senn::ipc::Connection *conn) {
  return new StatefulIMProxyIPC(conn);
}


StatefulIMProxyIPC::~StatefulIMProxyIPC() {
  if (connection_) {
    connection_->Close();
    delete connection_;
  }
}

} // fcitx
} // senn
