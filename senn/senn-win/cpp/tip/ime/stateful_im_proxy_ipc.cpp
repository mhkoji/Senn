#pragma once

#include <sstream>
#include "stateful_im_proxy_ipc.h"

namespace senn {
namespace senn_win {
namespace ime {

StatefulIMProxyIPC::StatefulIMProxyIPC(const HANDLE pipe) : pipe_(pipe) {
}

StatefulIMProxyIPC::~StatefulIMProxyIPC() {
  CloseHandle(pipe_);
}

void StatefulIMProxyIPC::Input(
    uint64_t keycode,
    std::function<void(const std::wstring* const text)> on_editing) {
  {
    std::stringstream ss;
    ss << "{"
       << "\"op\": \"input\","
       << "\"args\": {" << "\"keycode\": " << keycode
                        << "}"
       << "}";
    std::string req = ss.str();
    DWORD bytes_written;
    if (!WriteFile(pipe_, req.c_str(), static_cast<DWORD>(req.size()),
                   &bytes_written, NULL)) {
      return;
    }
  }

  std::string response;
  {
    char buf[1024] = { '\0' };
    DWORD bytes_read;
    if (!ReadFile(pipe_, buf, sizeof(buf), &bytes_read, NULL)) {
      return;
    }
    response = buf;
  }

  std::istringstream iss(response);
  std::string type;
  iss >> type;

  if (type == "EDITING") {
    std::string char_text;
    iss >> char_text;
    WCHAR text_buf[1024] = { '\0' };
    MultiByteToWideChar(CP_UTF8,
                        0,
                        char_text.c_str(),
                        static_cast<int>(char_text.length()),
                        text_buf,
                        static_cast<int>(sizeof(text_buf)));
    std::wstring text = text_buf;
    on_editing(&text);
  }
};

StatefulIMProxyIPC *StatefulIMProxyIPC::Create(
    const WCHAR* const named_pipe_path) {
  HANDLE pipe = CreateFile(
      named_pipe_path,
      GENERIC_READ | GENERIC_WRITE,
      0,
      NULL,
      OPEN_EXISTING, 
      FILE_ATTRIBUTE_NORMAL,
      NULL);	
  if (pipe == INVALID_HANDLE_VALUE) {
    return nullptr;
  }
  return new StatefulIMProxyIPC(pipe);
}

}  // ime
}  // senn_win
}  // senn
