#pragma once

#include <windows.h>
#include "stateful_im_proxy.h"

namespace senn {
namespace senn_win {
namespace ime {

class IPCStatefulIMProxy : public StatefulIMProxy {
public:
  bool Input(
      uint64_t keycode,
      std::function<void(const std::wstring* const text)>) override;

  ~IPCStatefulIMProxy() override;

private:
  IPCStatefulIMProxy(HANDLE);

  const HANDLE pipe_;

public:
  static IPCStatefulIMProxy *Create(const WCHAR* const named_pipe_path);
};

}  // ime
}  // senn_win
}  // senn
