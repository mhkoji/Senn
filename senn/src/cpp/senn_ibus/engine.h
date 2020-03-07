#pragma once
#include "ui.h"
#include "stateful_im_proxy_ipc.h"
#include "senn_fcitx/stateful_im_proxy_ipc_server.h"

namespace senn {
namespace ibus {
namespace engine {

struct IBusSennEngine {
  IBusEngine parent;
  senn::ibus::StatefulIMProxyIPC *im;
};


#define ENGINE(ptr) (reinterpret_cast<IBusSennEngine*>(ptr))

inline void Init(GTypeInstance *p, gpointer klass) {
  IBusSennEngine *engine = ENGINE(p);
  engine->im = NULL;
}

inline gboolean ProcessKeyEvent(
    IBusEngine *p,
    guint keyval,
    guint keycode,
    guint modifiers) {
  IBusSennEngine *engine = ENGINE(p);

  if (!engine->im) {
    engine->im = senn::ibus::StatefulIMProxyIPC::Create(
        senn::ipc::Connection::ConnectTo(5678));
  }

  return engine->im->Transit(keyval, keycode, modifiers,
    [&](const senn::fcitx::views::Converting *view) {
      senn::ibus::ui::Show(p, view);
    },

    [&](const senn::fcitx::views::Editing *view) {
      senn::ibus::ui::Show(p, view);
    });
}

} // engine
} // ibus
} // senn
