#pragma once
#include <fcitx/instance.h>
#include <string>

namespace hachee {
namespace fcitx {
namespace ui {

void CommitInput(FcitxInstance *instance,
                 const std::string &msg);

void UpdateInput(FcitxInstance *instance,
                 const std::string &msg, const int cursor_pos);

} // ui
} // fcitx
} // hachee
