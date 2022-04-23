#include <sstream>
#include <ws2tcpip.h>

#include "stateful_ime_proxy.h"
#include <picojson/picojson.h>

namespace senn {
namespace win {
namespace im {

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

ConnectionTCP::ConnectionTCP(SOCKET socket) : socket_(socket) {}

void ConnectionTCP::Close() { closesocket(socket_); }

bool ConnectionTCP::Write(const std::string &content) {
  const std::string data = content + "\n"; // Needs newline
  return send(socket_, data.c_str(), static_cast<int>(data.size()), 0) !=
         SOCKET_ERROR;
}

bool ConnectionTCP::ReadLine(std::string *output) {
  char buf[1024] = {'\0'};

  while (1) {
    int bytes_read = recv(socket_, buf, sizeof(buf), 0);
    if (bytes_read < 0) {
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

void StatefulIMEProxy::ToggleInputMode() {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"toggle-input-mode\""
       << "}";
    if (!conn_->Write(ss.str())) {
      return;
    }
    if (!conn_->ReadLine(&response)) {
      return;
    }
  }
  // It seems to need to consume output buffer...
  std::istringstream iss(response);
  std::string ok;
  iss >> ok;
}

InputMode StatefulIMEProxy::GetInputMode() {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"get-input-mode\""
       << "}";
    if (!conn_->Write(ss.str())) {
      return kUnknown;
    }
    if (!conn_->ReadLine(&response)) {
      return kUnknown;
    }
  }

  std::istringstream iss(response);
  std::string mode;
  iss >> mode;
  if (mode == "DIRECT") {
    return kDirect;
  }
  if (mode == "HIRAGANA") {
    return kHiragana;
  }

  return kUnknown;
}

bool StatefulIMEProxy::CanProcess(uint64_t keycode) {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"can-process\","
       << "\"args\": {"
       << "\"keycode\": " << keycode << "}"
       << "}";
    if (!conn_->Write(ss.str())) {
      return false;
    }
    if (!conn_->ReadLine(&response)) {
      return false;
    }
  }

  std::istringstream iss(response);
  bool can_process;
  iss >> can_process;
  return can_process;
}

bool StatefulIMEProxy::ProcessInput(
    uint64_t keycode, BYTE *modifiers,
    std::function<void(const views::Editing &)> on_editing,
    std::function<void(const views::Converting &)> on_converting,
    std::function<void(const views::Committed &)> on_committed) {
  std::string response;
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"process-input\","
       << "\"args\": {"
       << "\"keycode\": " << keycode << ","
       << "\"shift\": " << (modifiers[VK_SHIFT] ? "true" : "false") << "}"
       << "}";
    if (!conn_->Write(ss.str())) {
      return false;
    }
    if (!conn_->ReadLine(&response)) {
      return false;
    }
  }

  std::istringstream iss(response);
  bool eaten;
  iss >> eaten;

  if (eaten) {
    std::string type;
    iss >> type;

    if (type == "EDITING") {
      std::string content;
      std::getline(iss, content);

      views::Editing editing;
      {
        picojson::value v;
        picojson::parse(v, content);

        const std::string input =
            v.get<picojson::object>()["input"].get<std::string>();
        ToWString(input, &editing.input);

        const picojson::array predictions =
            v.get<picojson::object>()["predictions"].get<picojson::array>();
        for (picojson::array::const_iterator it = predictions.begin();
             it != predictions.end(); ++it) {
          std::wstring prediction;
          ToWString(it->get<std::string>(), &prediction);
          editing.predictions.push_back(prediction);
        }
      }

      on_editing(editing);
    } else if (type == "CONVERTING") {
      std::string content;
      std::getline(iss, content);

      views::Converting converting;
      {
        picojson::value v;
        picojson::parse(v, content);

        const picojson::array forms =
            v.get<picojson::object>()["forms"].get<picojson::array>();
        for (picojson::array::const_iterator it = forms.begin();
             it != forms.end(); ++it) {
          std::wstring form;
          ToWString(it->get<std::string>(), &form);
          converting.forms.push_back(form);
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
            static_cast<int>(v.get<picojson::object>()["cursor-form"]
                                 .get<picojson::object>()["candidate-index"]
                                 .get<double>());
      }

      on_converting(converting);
    } else if (type == "COMMITTED") {
      std::string content;
      std::getline(iss, content);

      views::Committed committed;
      {
        picojson::value v;
        picojson::parse(v, content);

        const std::string char_input =
            v.get<picojson::object>()["input"].get<std::string>();
        ToWString(char_input, &committed.input);
      }

      on_committed(committed);
    }
  }

  return eaten;
};

StatefulIMEProxy::StatefulIMEProxy(Connection *conn) : conn_(conn) {}

StatefulIMEProxy::~StatefulIMEProxy() { conn_->Close(); }

StatefulIMEProxy *
StatefulIMEProxy::CreateIPCPRoxy(const WCHAR *const named_pipe_path) {
  HANDLE pipe = CreateFile(named_pipe_path, GENERIC_READ | GENERIC_WRITE, 0,
                           NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (pipe == INVALID_HANDLE_VALUE) {
    return nullptr;
  }
  return new StatefulIMEProxy(new ConnectionIPC(pipe));
}

StatefulIMEProxy *StatefulIMEProxy::CreateTCPPRoxy(const std::string &host,
                                                   const std::string &port) {
  // https://docs.microsoft.com/ja-jp/windows/win32/winsock/complete-client-code
  struct addrinfo hint, *result = nullptr;
  ZeroMemory(&hint, sizeof(hint));
  hint.ai_family = AF_UNSPEC;
  hint.ai_socktype = SOCK_STREAM;
  hint.ai_protocol = IPPROTO_TCP;
  if (getaddrinfo(host.c_str(), port.c_str(), &hint, &result) != 0) {
    WSACleanup();
    return nullptr;
  }

  for (struct addrinfo *p = result; p != nullptr; p = p->ai_next) {
    SOCKET sock = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (sock == INVALID_SOCKET) {
      break;
    }
    if (connect(sock, p->ai_addr, static_cast<int>(p->ai_addrlen)) !=
        SOCKET_ERROR) {
      freeaddrinfo(result);
      return new StatefulIMEProxy(new ConnectionTCP(sock));
    }
    closesocket(sock);
  }
  freeaddrinfo(result);
  WSACleanup();
  return nullptr;
}

} // namespace im
} // namespace win
} // namespace senn
