#pragma once

#include <string>
#include <vector>

namespace senn {
namespace win {
namespace im {
namespace views {

struct Editing {
  std::wstring input;
  std::vector<std::wstring> predictions;
};

struct Converting {
  std::vector<std::wstring> forms;
  size_t cursor_form_index;
  std::vector<std::wstring> cursor_form_candidates;
  int cursor_form_candidate_index;
};

struct Committed {
  std::wstring input;
};

} // namespace views
} // namespace im
} // namespace win
} // namespace senn
