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
  int cursor_pos;
};


struct Committed {
  std::string input;
};

} // states
} // fcitx
} // senn
