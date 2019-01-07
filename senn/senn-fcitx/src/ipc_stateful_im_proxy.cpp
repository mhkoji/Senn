#include <fcitx/instance.h>
#include <sstream>
#include <picojson/picojson.h>

#include "ipc_stateful_im_proxy.h"

namespace senn {
namespace fcitx {

namespace {

void ParseCommitted(std::istringstream &content,
                    senn::fcitx::views::Committed *output) {
  std::string input;
  content >> input;
  output->input = input;
}

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

void ParseEditing(std::istringstream &content,
                  senn::fcitx::views::Editing *output) {
  int cursor_pos;
  std::string input;
  content >> cursor_pos;
  content >> input;
  output->cursor_pos = cursor_pos;
  output->input = input;
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


IPCStatefulIMProxy::IPCStatefulIMProxy(senn::ipc::Connection* conn)
  : connection_(conn) {}


INPUT_RETURN_VALUE
IPCStatefulIMProxy::Input(
    FcitxKeySym code,
    std::function<void(const senn::fcitx::views::Committed*)> on_committed,
    std::function<void(const senn::fcitx::views::Converting*)> on_converting,
    std::function<void(const senn::fcitx::views::Editing*)> on_editing) {
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"input\","
       << "\"args\": {" << "\"code\": " << code << "}"
       << "}\n";
    connection_->Write(ss.str());
  }

  std::string result;
  connection_->ReadLine(&result);

  std::string input_return_value, type;
  std::istringstream iss(result);
  iss >> input_return_value;
  iss >> type;

  if (type == "COMMITTED") {
    senn::fcitx::views::Committed committed;
    ParseCommitted(iss, &committed);
    on_committed(&committed);
  } else if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    picojson::value v;
    picojson::parse(v, content);

    senn::fcitx::views::Converting converting;
    ParseConverting(v, &converting);
    on_converting(&converting);
  } else {
    senn::fcitx::views::Editing editing;
    ParseEditing(iss, &editing);
    on_editing(&editing);
  }

  return ParseInputReturnValue(input_return_value);
}

IPCStatefulIMProxy*
IPCStatefulIMProxy::Create(const std::string &socket_name) {
  return new IPCStatefulIMProxy(senn::ipc::Connection::ConnectTo(socket_name));
}


IPCStatefulIMProxy::~IPCStatefulIMProxy() {
  if (connection_) {
    connection_->Close();
  }
}

} // fcitx
} // senn
