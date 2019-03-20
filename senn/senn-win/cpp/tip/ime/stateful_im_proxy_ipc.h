#pragma once

#include <windows.h>
#include "stateful_im.h"

namespace senn {
namespace senn_win {
namespace ime {

class StatefulIMProxyIPC : public StatefulIM {
public:
  void Transit(
      uint64_t keycode,
      std::function<void(const views::Editing&)>,
      std::function<void(const views::Committed&)>) override;

  ~StatefulIMProxyIPC() override;

private:
  StatefulIMProxyIPC(HANDLE);

  const HANDLE pipe_;

public:
  static StatefulIMProxyIPC *Create(const WCHAR* const named_pipe_path);
};

}  // ime
}  // senn_win
}  // senn
