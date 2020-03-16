#pragma once
#include "ui.h"
#include "stateful_im_proxy_ipc.h"
#include "senn_fcitx/stateful_im_proxy_ipc_server.h"

namespace senn {
namespace ibus {
namespace engine {


typedef senn::ipc::ConnectionFactory*
  (*InitCommunicationToBackendServer)();

inline
senn::ipc::ConnectionFactory* ServerLaunchInitializer() {
  senn::fcitx::StatefulIMProxyIPCServerLauncher *launcher =
    new senn::fcitx::StatefulIMProxyIPCServerLauncher();
  launcher->Spawn();
  return launcher;
}

inline
senn::ipc::ConnectionFactory* ServerConnectInitializer() {
  return new senn::ipc::TcpConnectionFactory(5678);
}

struct EngineClass {
  IBusEngineClass parent;
  InitCommunicationToBackendServer init_comm_fn;
};

struct Engine {
  IBusEngine parent;
  senn::ipc::ConnectionFactory *connection_factory;
  senn::ibus::StatefulIM *im;
};


#define ENGINE(ptr) (reinterpret_cast<Engine*>(ptr))

inline void Init(GTypeInstance *p, gpointer klass) {
  Engine *engine = ENGINE(p);
  EngineClass *engine_class = G_TYPE_CHECK_CLASS_CAST(klass,
                                                      IBUS_TYPE_ENGINE,
                                                      EngineClass);
  engine->im = NULL;
  engine->connection_factory = engine_class->init_comm_fn();
}

inline gboolean ProcessKeyEvent(
    IBusEngine *p,
    guint keyval,
    guint keycode,
    guint modifiers) {
  Engine *engine = ENGINE(p);

  if (!engine->im) {
    engine->im = senn::ibus::StatefulIMProxyIPC::Create(
        engine->connection_factory->Create());
  }

  const bool is_key_up = ((modifiers & IBUS_RELEASE_MASK) != 0);
  if (is_key_up) {
    return FALSE;
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
