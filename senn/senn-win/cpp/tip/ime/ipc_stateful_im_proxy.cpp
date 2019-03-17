#pragma once

#include "ipc_stateful_im_proxy.h"

namespace senn {
namespace senn_win {
namespace ime {

IPCStatefulIMProxy::IPCStatefulIMProxy(
    const WCHAR* const named_pipe_path)
  : named_pipe_path_(named_pipe_path) {
}

bool IPCStatefulIMProxy::Input(
    uint64_t keycode,
    std::function<void(const std::wstring* const text)> on_editing) {
  text_ += WCHAR(keycode);
  on_editing(&text_);
  return true;
};

}  // ime
}  // senn_win
}  // senn
