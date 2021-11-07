#pragma once
#include <functional>
#include <string>

#include "senn_fcitx/views.h"

namespace senn {
namespace ibus {

class StatefulIM {
public:
  virtual ~StatefulIM() {}

  virtual bool
  Transit(unsigned int, unsigned int, unsigned int,
          std::function<void(const senn::fcitx::views::Converting *)>,
          std::function<void(const senn::fcitx::views::Editing *)>) = 0;
};

} // namespace ibus
} // namespace senn
