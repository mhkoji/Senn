#pragma once
#include <fcitx/instance.h>
#include <vector>
#include <string>

#include "views.h"

namespace senn {
namespace fcitx {
namespace ui {

void Show(FcitxInstance*, const senn::fcitx::views::Converting*);

void Show(FcitxInstance*, const senn::fcitx::views::Editing*);

} // ui
} // fcitx
} // senn
