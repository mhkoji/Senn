#pragma once

#include <string>
#include <vector>

namespace senn {
namespace senn_win {
namespace ime {
namespace views {

struct Editing {
  std::wstring input;
};

struct Converting {
  std::vector<std::wstring> forms;
  size_t cursor_form_index;

  Converting() : cursor_form_index(0) {}
};

struct Committed {
  std::wstring input;
};

} // namespace views
} // namespace ime
} // namespace senn_win
} // namespace senn
