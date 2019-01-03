#pragma once
#include <fcitx/instance.h>
#include <vector>
#include <string>

namespace senn {
namespace fcitx {
namespace ui {

void Committed(FcitxInstance *,
               const std::string &, const int);

void Converting(FcitxInstance *,
                const std::vector<std::string>&, const int);

void Editing(FcitxInstance *,
             const std::string &, const int);

} // ui
} // fcitx
} // senn
