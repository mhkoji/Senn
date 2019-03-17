#pragma once

#include <windows.h>
#include "stateful_im_proxy.h"

namespace senn {
namespace senn_win {
namespace ime {

class IPCStatefulIMProxy : public StatefulIMProxy {
public:
  IPCStatefulIMProxy(const WCHAR* const named_pipe_path);

  bool Input(
      uint64_t keycode,
      std::function<void(const std::wstring* const text)>) override;

private:
  const WCHAR* const named_pipe_path_;

  std::wstring text_ = L"";
};

}  // ime
}  // senn_win
}  // senn
