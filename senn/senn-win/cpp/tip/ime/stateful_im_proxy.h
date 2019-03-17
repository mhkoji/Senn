#pragma once

#include <functional>

namespace senn {
namespace senn_win {
namespace ime {

class StatefulIMProxy {
public:
  virtual ~StatefulIMProxy() {}

  virtual bool Input(
      uint64_t keycode,
      std::function<void(const std::wstring* const text)>) = 0;
};

}  // ime
}  // senn_win
}  // senn
