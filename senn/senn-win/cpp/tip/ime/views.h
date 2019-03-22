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
};

struct Committed {
  std::wstring input;
};

} // views
} // ime
} // senn_win 
} // senn
