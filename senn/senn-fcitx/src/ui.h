#pragma once
#include <fcitx/instance.h>
#include <vector>
#include <string>

#include "states.h"

namespace senn {
namespace fcitx {
namespace ui {

void Committed(FcitxInstance *, const senn::fcitx::states::Committed*);

void Converting(FcitxInstance *, const senn::fcitx::states::Converting*);

void Editing(FcitxInstance *, const senn::fcitx::states::Editing*);

} // ui
} // fcitx
} // senn
