#include "engine.h"

namespace senn {
namespace ibus {
namespace engine {

#define ENGINE(ptr) (reinterpret_cast<IBusSennEngine*>(ptr))

gboolean ProcessKeyEvent(
    IBusEngine *p,
    guint keyval,
    guint keycode,
    guint modifiers) {
  return ENGINE(p)->im->Transit(keyval, keycode, modifiers,
    [&](const senn::fcitx::views::Converting *view) {
      Show(view);
    },

    [&](const senn::fcitx::views::Editing *view) {
      Show(view);
    });
}

} // engine
} // ibus
} // senn
