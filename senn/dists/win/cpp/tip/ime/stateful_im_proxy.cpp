#include <sstream>

#include "../../third-party/picojson/picojson.h"
#include "stateful_im_proxy.h"

namespace senn {
namespace senn_win {
namespace ime {

ConnectionIPC::ConnectionIPC(const HANDLE pipe) : pipe_(pipe) {}

void ConnectionIPC::Close() { CloseHandle(pipe_); }

bool ConnectionIPC::Write(const std::string &content) {
  DWORD bytes_written;
  return WriteFile(pipe_, content.c_str(), static_cast<DWORD>(content.size()),
                   &bytes_written, NULL);
}

bool ConnectionIPC::ReadLine(std::string *output) {
  char buf[1024] = {'\0'};

  while (1) {
    DWORD bytes_read;
    if (!ReadFile(pipe_, buf, sizeof(buf), &bytes_read, NULL)) {
      return false;
    }

    *output += std::string(buf, bytes_read);

    if (buf[bytes_read - 1] == '\n') {
      break;
    }
  }

  return true;
}

namespace {

int ToWString(const std::string &char_string, std::wstring *output) {
  WCHAR buf[1024] = {'\0'};
  int size = MultiByteToWideChar(CP_UTF8, 0, char_string.c_str(),
                                 static_cast<int>(char_string.length()), buf,
                                 static_cast<int>(sizeof(buf)));
  *output = buf;
  return size;
}

} // namespace

void StatefulIMProxy::Transit(
    uint64_t keycode, std::function<void(const views::Editing &)> on_editing,
    std::function<void(const views::Converting &)> on_converting,
    std::function<void(const views::Committed &)> on_committed) {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"process-input\","
       << "\"args\": {"
       << "\"keycode\": " << keycode << "}"
       << "}";
    if (!conn_->Write(ss.str())) {
      return;
    }
    if (!conn_->ReadLine(&response)) {
      return;
    }
  }

  std::istringstream iss(response);
  std::string type;
  iss >> type;

  if (type == "EDITING") {
    views::Editing editing;
    std::string char_text;
    iss >> char_text;
    ToWString(char_text, &editing.input);
    on_editing(editing);
  } else if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    views::Converting converting;

    picojson::value v;
    picojson::parse(v, content);

    {
      const picojson::array forms =
          v.get<picojson::object>()["forms"].get<picojson::array>();
      for (picojson::array::const_iterator it = forms.begin();
           it != forms.end(); ++it) {
        std::wstring form;
        ToWString(it->get<std::string>(), &form);
        converting.forms.push_back(form);
      }
    }

    converting.cursor_form_index = static_cast<size_t>(
        v.get<picojson::object>()["cursor-form-index"].get<double>());

    {
      const picojson::array candidates =
          v.get<picojson::object>()["cursor-form"]
              .get<picojson::object>()["candidates"]
              .get<picojson::array>();
      for (picojson::array::const_iterator it = candidates.begin();
           it != candidates.end(); ++it) {
        std::wstring str;
        ToWString(it->get<std::string>(), &str);
        converting.cursor_form_candidates.push_back(str);
      }
    }

    converting.cursor_form_candidate_index =
        static_cast<size_t>(v.get<picojson::object>()["cursor-form"]
                                .get<picojson::object>()["candidate-index"]
                                .get<double>());

    on_converting(converting);
  } else if (type == "COMMITTED") {
    std::string content;
    std::getline(iss, content);

    views::Committed committed;

    picojson::value v;
    picojson::parse(v, content);

    const std::string char_input =
        v.get<picojson::object>()["input"].get<std::string>();
    ToWString(char_input, &committed.input);

    on_committed(committed);
  }
};

StatefulIMProxy::StatefulIMProxy(Connection *conn) : conn_(conn) {}

StatefulIMProxy::~StatefulIMProxy() { conn_->Close(); }

StatefulIMProxy *
StatefulIMProxy::CreateIPCPRoxy(const WCHAR *const named_pipe_path) {
  HANDLE pipe = CreateFile(named_pipe_path, GENERIC_READ | GENERIC_WRITE, 0,
                           NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (pipe == INVALID_HANDLE_VALUE) {
    return nullptr;
  }
  return new StatefulIMProxy(new ConnectionIPC(pipe));
}

} // namespace ime
} // namespace senn_win
} // namespace senn
