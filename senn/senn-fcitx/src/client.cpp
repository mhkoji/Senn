#include <fcitx/instance.h>
#include <sstream>
#include <picojson/picojson.h>

#include "client.h"
#include "ipc.h"

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
  output->cursor_pos = content
    .get<picojson::object>()["current"]
    .get<picojson::object>()["cursor-pos"]
    .get<double>();

  const picojson::array forms = content
    .get<picojson::object>()["forms"]
    .get<picojson::array>();
  for (picojson::array::const_iterator it = forms.begin();
       it != forms.end(); ++it) {
    output->forms.push_back(it->get<std::string>());
  }
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


Client::Client()
  : connection_(nullptr) {}


INPUT_RETURN_VALUE
Client::DoInput(
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
