#include <fcitx/instance.h>
#include <sstream>
#include <picojson/picojson.h>

#include "ipc_client.h"

namespace senn {
namespace fcitx {

namespace {

void ParseCommitted(std::istringstream &content,
                    senn::fcitx::states::Committed *output) {
  std::string input;
  content >> input;
  output->input = input;
}

void ParseConverting(picojson::value &content,
                     senn::fcitx::states::Converting *output) {
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
                  senn::fcitx::states::Editing *output) {
  int consumed, cursor_pos;
  std::string input;
  content >> consumed;
  content >> cursor_pos;
  content >> input;
  output->consumed = (consumed == 1);
  output->input = input;
  output->cursor_pos = cursor_pos;
}

} // namespace


IPCClient::IPCClient(senn::ipc::Connection* conn)
  : connection_(conn) {}


INPUT_RETURN_VALUE
IPCClient::TransitByInput(
    FcitxKeySym code,
    std::function<INPUT_RETURN_VALUE(
        const senn::fcitx::states::Committed*)> on_committed,
    std::function<INPUT_RETURN_VALUE(
        const senn::fcitx::states::Converting*)> on_converting,
    std::function<INPUT_RETURN_VALUE(
        const senn::fcitx::states::Editing*)> on_editing) {
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"transit-by-input\","
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
    senn::fcitx::states::Committed committed;
    ParseCommitted(iss, &committed);
    return on_committed(&committed);
  }

  if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    picojson::value v;
    picojson::parse(v, content);

    senn::fcitx::states::Converting converting;
    ParseConverting(v, &converting);
    return on_converting(&converting);
  }

  senn::fcitx::states::Editing editing;
  ParseEditing(iss, &editing);
  return on_editing(&editing);
}

IPCClient* IPCClient::Create(const std::string &socket_name) {
  return new IPCClient(senn::ipc::Connection::ConnectTo(socket_name));
}


IPCClient::~IPCClient() {
  if (connection_) {
    connection_->Close();
  }
}

} // fcitx
} // senn
