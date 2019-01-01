#pragma once
#include <fcitx/instance.h>
#include <string>

namespace hachee {
namespace fcitx {
namespace ui {

void CommitInput(FcitxInstance *, const std::string &, const int);

void UpdateInput(FcitxInstance *, const std::string &, const int);

} // ui
} // fcitx
} // hachee
