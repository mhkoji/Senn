#pragma once
#include <string>
#include <vector>

namespace senn {
namespace fcitx {
namespace im {
namespace views {

struct Editing {
  std::string input;
  int cursor_pos;
  std::vector<std::string> predictions;
  int prediction_index;
  std::string committed_input;
};

struct Converting {
  std::vector<std::string> forms;
  int cursor_form_index;
  std::vector<std::string> cursor_form_candidates;
  int cursor_form_candidate_index;
};

} // namespace views
} // namespace im
} // namespace fcitx
} // namespace senn
