#pragma once

#include <windows.h>
#include "stateful_im.h"

namespace senn {
namespace senn_win {
namespace ime {

class StatefulIMIPCProxy : public StatefulIM {
public:
  bool Input(
      uint64_t keycode,
      std::function<void(const std::wstring* const text)>) override;

  ~StatefulIMIPCProxy() override;

private:
  StatefulIMIPCProxy(HANDLE);

  const HANDLE pipe_;

public:
  static StatefulIMIPCProxy *Create(const WCHAR* const named_pipe_path);
};

}  // ime
}  // senn_win
}  // senn
