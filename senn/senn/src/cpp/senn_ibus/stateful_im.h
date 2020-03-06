#pragma once
#include <string>
#include <functional>

#include "senn_fcitx/views.h"

namespace senn {
namespace ibus {

class StatefulIM {
public:
  virtual ~StatefulIM() {}

  virtual boolean Transit(
      guint, guint, guint,
      std::function<void(const senn::fcitx::views::Converting*)>,
      std::function<void(const senn::fcitx::views::Editing*)>) = 0;
};

} // ibus
} // senn
