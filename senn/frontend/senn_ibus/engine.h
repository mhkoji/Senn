#pragma once
#include "senn_fcitx/stateful_im_proxy_ipc_server.h"
#include "stateful_im_proxy_ipc.h"
#include "ui.h"

namespace senn {
namespace ibus {
namespace engine {

class BackendCommunication {
public:
  virtual ~BackendCommunication() {}

  virtual senn::ipc::RequesterInterface *GetRequester() = 0;
};

typedef BackendCommunication *(*CreatBackendCommunication)();

class BackendCommunicationConnect : public BackendCommunication {
public:
  ~BackendCommunicationConnect() { delete factory_; }

  senn::ipc::RequesterInterface *GetRequester() {
    return new senn::ipc::Requester(factory_);
  }

private:
  BackendCommunicationConnect(senn::ipc::ConnectionFactory *factory)
      : factory_(factory) {}

  senn::ipc::ConnectionFactory *factory_;

public:
  static BackendCommunication *Create() {
    return new BackendCommunicationConnect(
        new senn::ipc::TcpConnectionFactory(5678));
  }
};

class BackendCommunicationLaunch : public BackendCommunication {
public:
  ~BackendCommunicationLaunch() { delete launcher_; }

  senn::ipc::RequesterInterface *GetRequester() {
    return new senn::fcitx::ReconnectableStatefulIMRequester(launcher_);
  }

private:
  BackendCommunicationLaunch(
      senn::fcitx::StatefulIMProxyIPCServerLauncher *launcher)
      : launcher_(launcher) {}

  senn::fcitx::StatefulIMProxyIPCServerLauncher *launcher_;

public:
  static BackendCommunication *Create() {
    senn::fcitx::StatefulIMProxyIPCServerLauncher *launcher =
        new senn::fcitx::StatefulIMProxyIPCServerLauncher(
            "/usr/lib/senn/server");

    launcher->Spawn();

    return new BackendCommunicationLaunch(launcher);
  }
};

struct EngineClass {
  IBusEngineClass parent;
  BackendCommunication *backend_comm;
};

struct Engine {
  IBusEngine parent;
  senn::ibus::StatefulIM *im;
};

#define ENGINE(ptr) (reinterpret_cast<Engine *>(ptr))

inline void Init(GTypeInstance *p, gpointer klass) {
  Engine *engine = ENGINE(p);
  EngineClass *engine_class =
      G_TYPE_CHECK_CLASS_CAST(klass, IBUS_TYPE_ENGINE, EngineClass);
  engine->im = new senn::ibus::StatefulIMProxyIPC(
      std::unique_ptr<senn::ipc::RequesterInterface>(
          engine_class->backend_comm->GetRequester()));
}

inline gboolean ProcessKeyEvent(IBusEngine *p, guint keyval, guint keycode,
                                guint modifiers) {
  Engine *engine = ENGINE(p);

  const bool is_key_up = ((modifiers & IBUS_RELEASE_MASK) != 0);
  if (is_key_up) {
    return FALSE;
  }

  return engine->im->Transit(
      keyval, keycode, modifiers,
      [&](const senn::fcitx::views::Converting *view) {
        senn::ibus::ui::Show(p, view);
      },

      [&](const senn::fcitx::views::Editing *view) {
        senn::ibus::ui::Show(p, view);
      });
}

} // namespace engine
} // namespace ibus
} // namespace senn
