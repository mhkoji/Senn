#pragma once
#include <fcitx/instance.h>
#include <fcitx/ui.h>
#include <vector>
#include <string>

#include "views.h"

namespace senn {
namespace fcitx {
namespace ui {

void Show(FcitxInstance*, const senn::fcitx::views::Converting*);

void Show(FcitxInstance*, const senn::fcitx::views::Editing*);

void SetupMenu(FcitxInstance*, FcitxUIMenu *);

void DestoryMenu(FcitxInstance*, FcitxUIMenu*);

void SetMenuVisibility(FcitxInstance*, boolean);

} // ui
} // fcitx
} // senn
