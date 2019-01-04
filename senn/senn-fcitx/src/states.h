#pragma once
#include <string>
#include <vector>

namespace senn {
namespace fcitx {
namespace states {

struct Editing {
  boolean consumed;
  std::string input;
  int cursor_pos;
};


struct Converting {
  std::vector<std::string> forms;
  int cursor_form_index;
  std::vector<std::string> cursor_form_candidates;
  int cursor_form_candidate_index;
};


struct Committed {
  std::string input;
};

} // states
} // fcitx
} // senn
