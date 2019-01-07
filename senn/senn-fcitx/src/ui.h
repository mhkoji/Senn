#pragma once
#include <fcitx/instance.h>
#include <vector>
#include <string>

#include "views.h"

namespace senn {
namespace fcitx {
namespace ui {

void Draw(FcitxInstance*, const senn::fcitx::views::Committed*);

void Draw(FcitxInstance*, const senn::fcitx::views::Converting*);

void Draw(FcitxInstance*, const senn::fcitx::views::Editing*);

} // ui
} // fcitx
} // senn
