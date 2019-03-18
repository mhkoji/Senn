#pragma once
#include <string>

namespace senn {
namespace senn_win {
namespace ime {
namespace views {

struct Editing {
  std::wstring input;
};

struct Committed {
  std::wstring input;
};

} // views
} // ime
} // senn_win 
} // senn
