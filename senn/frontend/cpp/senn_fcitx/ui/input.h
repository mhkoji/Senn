#pragma once
#include <fcitx/instance.h>
#include <fcitx/ui.h>
#include <vector>
#include <string>
#include "../im/views.h"

namespace senn {
namespace fcitx {
namespace ui {
namespace input {

void Show(FcitxInstance*, const senn::fcitx::im::views::Converting*);
void Show(FcitxInstance*, const senn::fcitx::im::views::Editing*);

} // input
} // ui
} // fcitx
} // senn
