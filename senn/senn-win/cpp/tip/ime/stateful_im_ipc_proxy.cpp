#pragma once

#include <sstream>
#include "stateful_im_ipc_proxy.h"

namespace senn {
namespace senn_win {
namespace ime {

StatefulIMIPCProxy::StatefulIMIPCProxy(const HANDLE pipe) : pipe_(pipe) {
}

StatefulIMIPCProxy::~StatefulIMIPCProxy() {
  CloseHandle(pipe_);
}

bool StatefulIMIPCProxy::Input(
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

StatefulIMIPCProxy *StatefulIMIPCProxy::Create(
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
  return new StatefulIMIPCProxy(pipe);
}

}  // ime
}  // senn_win
}  // senn
