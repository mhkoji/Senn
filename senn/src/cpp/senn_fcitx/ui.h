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


class MenuHandlerInterface {
public:
  virtual boolean OnAbout() = 0;
};

void SetupMenu(FcitxInstance*, FcitxUIMenu *, MenuHandlerInterface*);

void DestoryMenu(FcitxInstance*, FcitxUIMenu*);

void SetMenuVisibility(FcitxInstance*, boolean);

} // ui
} // fcitx
} // senn
