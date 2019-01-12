#pragma once
#include <string>

namespace senn {
namespace fcitx {

// The server must prevent the double startup by itself.
bool StartIPCServer(const std::string&);

} // fcitx
} // senn
