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

bool StatefulIMProxyIPC::Input(
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
      return false;
    }
  }

  std::wstring text;
  {
    char buf[1024] = { '\0' };
    DWORD bytes_read;
    if (!ReadFile(pipe_, buf, sizeof(buf), &bytes_read, NULL)) {
      return false;
    }

    WCHAR w_buf[1024] = { '\0' };
    MultiByteToWideChar(CP_UTF8, 0, buf, bytes_read, w_buf, sizeof(w_buf));
    text = w_buf;
  }

  on_editing(&text);
  
  return true;
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
