#include <glib-object.h>
#include <ibus.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>
#include <iostream>

#include "ipc/ipc.h"
#include "ipc/request.h"
#include "senn_fcitx/im/stateful_ime.h"
#include "senn_fcitx/im/stateful_ime_proxy_ipc.h"
#include "senn_fcitx/im/stateful_ime_proxy_ipc_server.h"

namespace senn {
namespace ibus_senn {
namespace engine {

class BackendCommunication {
public:
  virtual ~BackendCommunication() {}

  virtual senn::ipc::RequesterInterface *GetRequester() = 0;
};

struct EngineClass {
  IBusEngineClass parent;
  BackendCommunication *backend_comm;
};

struct Engine {
  IBusEngine parent;
  senn::fcitx::im::StatefulIME *ime;
};

#define ENGINE(ptr) (reinterpret_cast<Engine *>(ptr))

void Init(GTypeInstance *p, gpointer klass) {
  Engine *engine = ENGINE(p);
  EngineClass *engine_class =
      G_TYPE_CHECK_CLASS_CAST(klass, IBUS_TYPE_ENGINE, EngineClass);
  engine->ime = new senn::fcitx::im::StatefulIMEProxyIPC(
      std::unique_ptr<senn::ipc::RequesterInterface>(
          engine_class->backend_comm->GetRequester()));
}

void Show(IBusEngine *engine,
          const senn::fcitx::im::views::Converting *converting) {
  IBusText *text = nullptr;
  {
    std::string data;
    std::vector<std::string>::const_iterator it = converting->forms.begin();
    for (; it != converting->forms.end(); ++it) {
      data.append(*it);
    }
    text = ibus_text_new_from_string(data.c_str());
    ibus_text_append_attribute(text, IBUS_ATTR_TYPE_UNDERLINE,
                               IBUS_ATTR_UNDERLINE_SINGLE, 0,
                               g_utf8_strlen(data.c_str(), -1));
  }

  int cursor_pos = 0;
  {
    int start = 0, end = 0;
    int i = 0, cursor_form_index = converting->cursor_form_index;
    std::vector<std::string>::const_iterator it = converting->forms.begin();
    for (; it != converting->forms.end(); ++it, ++i) {
      end += g_utf8_strlen(it->c_str(), -1);
      if (i == cursor_form_index) {
        cursor_pos = start;
        const guint kBackgroundColor = 0xD1EAFF;
        ibus_text_append_attribute(text, IBUS_ATTR_TYPE_BACKGROUND,
                                   kBackgroundColor, start, end);
        // IBUS_ATTR_TYPE_FOREGROUND is necessary to highlight the segment on
        // Firefox.
        const guint kForegroundColor = 0x000000;
        ibus_text_append_attribute(text, IBUS_ATTR_TYPE_FOREGROUND,
                                   kForegroundColor, start, end);
      }
      start = end;
    }
  }

  ibus_engine_update_preedit_text_with_mode(engine, text, cursor_pos, TRUE,
                                            IBUS_ENGINE_PREEDIT_COMMIT);
}

void Show(IBusEngine *engine, const senn::fcitx::im::views::Editing *editing) {
  if (editing->committed_input != "") {
    IBusText *committed_text =
        ibus_text_new_from_string(editing->committed_input.c_str());
    ibus_engine_commit_text(engine, committed_text);
  }

  if (editing->input == "") {
    // Without this, the layout of the input console breaks
    // (The cursor is not shown, for example)
    ibus_engine_hide_preedit_text(engine);
    return;
  }

  IBusText *input_text = ibus_text_new_from_string(editing->input.c_str());
  ibus_text_append_attribute(input_text, IBUS_ATTR_TYPE_UNDERLINE,
                             IBUS_ATTR_UNDERLINE_SINGLE, 0,
                             g_utf8_strlen(editing->input.c_str(), -1));

  ibus_engine_update_preedit_text_with_mode(
      engine, input_text,
      g_utf8_strlen(editing->input.c_str(), editing->cursor_pos), TRUE,
      IBUS_ENGINE_PREEDIT_COMMIT);
}

gboolean ProcessKeyEvent(IBusEngine *p, guint keyval, guint keycode,
                         guint modifiers) {
  const bool is_key_up = ((modifiers & IBUS_RELEASE_MASK) != 0);
  if (is_key_up) {
    return FALSE;
  }

  return ENGINE(p)->ime->ProcessInput(
      keyval, keycode, modifiers,
      [&](const senn::fcitx::im::views::Converting *view) {
        Show(p, view);
      },

      [&](const senn::fcitx::im::views::Editing *view) {
        Show(p, view);
      });
}

} // namespace engine
} // namespace ibus_senn
} // namespace senn

namespace {

class BackendCommunicationConnect
    : public senn::ibus_senn::engine::BackendCommunication {
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

class BackendCommunicationLaunch
    : public senn::ibus_senn::engine::BackendCommunication {
public:
  ~BackendCommunicationLaunch() { delete launcher_; }

  senn::ipc::RequesterInterface *GetRequester() {
    return new senn::fcitx::im::ReconnectableStatefulIMERequester(launcher_);
  }

private:
  BackendCommunicationLaunch(
                             senn::fcitx::im::StatefulIMEProxyIPCServerLauncher *launcher)
      : launcher_(launcher) {}

  senn::fcitx::im::StatefulIMEProxyIPCServerLauncher *launcher_;

public:
  static BackendCommunication *Create() {
    senn::fcitx::im::StatefulIMEProxyIPCServerLauncher *launcher =
      new senn::fcitx::im::StatefulIMEProxyIPCServerLauncher(
            "/usr/lib/senn/server");

    launcher->Spawn();

    return new BackendCommunicationLaunch(launcher);
  }
};

typedef senn::ibus_senn::engine::BackendCommunication *(
    *CreatBackendCommunication)();

IBusEngineClass *g_parent_class = NULL;

CreatBackendCommunication g_create_backend_comm_fn = NULL;

GObject *
SennEngineClassConstructor(GType type, guint n_construct_properties,
                           GObjectConstructParam *construct_properties) {
  return G_OBJECT_CLASS(g_parent_class)
      ->constructor(type, n_construct_properties, construct_properties);
}

void SennEngineClassDestroy(IBusObject *engine) {
  IBUS_OBJECT_CLASS(g_parent_class)->destroy(engine);
}

void SennEngineClassInit(gpointer klass, gpointer class_data) {
  IBusEngineClass *engine_class = IBUS_ENGINE_CLASS(klass);
  engine_class->process_key_event = senn::ibus_senn::engine::ProcessKeyEvent;

  g_parent_class =
      reinterpret_cast<IBusEngineClass *>(g_type_class_peek_parent(klass));

  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->constructor = SennEngineClassConstructor;

  IBusObjectClass *ibus_object_class = IBUS_OBJECT_CLASS(klass);
  ibus_object_class->destroy = SennEngineClassDestroy;

  // Senn-related initialization
  senn::ibus_senn::engine::EngineClass *senn_engine_class =
      G_TYPE_CHECK_CLASS_CAST(klass, IBUS_TYPE_ENGINE,
                              senn::ibus_senn::engine::EngineClass);
  senn_engine_class->backend_comm = g_create_backend_comm_fn();
}

GType GetType() {
  static GType type = 0;

  static const GTypeInfo type_info = {
      sizeof(senn::ibus_senn::engine::EngineClass),
      NULL,
      NULL,
      SennEngineClassInit,
      NULL,
      NULL,
      sizeof(senn::ibus_senn::engine::Engine),
      0,
      senn::ibus_senn::engine::Init,
  };

  if (type == 0) {
    type = g_type_register_static(IBUS_TYPE_ENGINE, "IBusSennEngine",
                                  &type_info, static_cast<GTypeFlags>(0));
    if (type == 0) {
      std::cerr << "g_type_register_static failed";
      exit(1);
    }
  }

  return type;
}

void Disconnected(IBusBus *bus, gpointer user_data) { ibus_quit(); }

void StartEngine(bool exec_by_daemon) {
  ibus_init();

  IBusBus *bus = ibus_bus_new();
  g_signal_connect(bus, "disconnected", G_CALLBACK(Disconnected), NULL);

  {
    IBusComponent *component = ibus_component_new(
        "org.freedesktop.IBus.Senn", "Senn Component", PACKAGE_VERSION, "MIT",
        "mhkoji", "", "", "ibus-senn");
    ibus_component_add_engine(
        component, ibus_engine_desc_new("senn-jp", "Senn", "Senn Component",
                                        "ja", "MIT", "mhkoji", "", "default"));

    {
      IBusFactory *factory = ibus_factory_new(ibus_bus_get_connection(bus));
      ibus_factory_add_engine(factory, "senn-jp", GetType());
    }

    if (exec_by_daemon) {
      ibus_bus_request_name(bus, "org.freedesktop.IBus.Senn", 0);
    } else {
      ibus_bus_register_component(bus, component);
    }

    g_object_unref(component);
  }

  ibus_main();
}

} // namespace

namespace {

gboolean g_option_ibus = FALSE;
gchar *g_option_backend_init = NULL;

const GOptionEntry g_option_entries[] = {
    {"ibus", 'i', 0, G_OPTION_ARG_NONE, &g_option_ibus,
     "Component is executed by ibus", NULL},
    {"backend-comm", 0, 0, G_OPTION_ARG_STRING, &g_option_backend_init,
     "Specify a way to communicate with the backend server", NULL},
    {NULL},
};

class ContextReleaser {
public:
  ContextReleaser(GOptionContext *context) : context_(context) {}

  ~ContextReleaser() { g_option_context_free(context_); }

private:
  GOptionContext *context_;
};

} // namespace

int main(gint argc, gchar **argv) {
  // A global variable used during senn::ibus::engine::Init
  g_create_backend_comm_fn = BackendCommunicationLaunch::Create;
  {
    GOptionContext *context =
        g_option_context_new("- ibus senn engine component");
    ContextReleaser releaser(context);

    g_option_context_add_main_entries(context, g_option_entries, "ibus-senn");
    GError *error = NULL;
    if (!g_option_context_parse(context, &argc, &argv, &error)) {
      g_print("Option parsing failed: %s\n", error->message);
      std::exit(1);
    }

    if (g_option_backend_init) {
      if (strcmp(g_option_backend_init, "connect") == 0) {
        g_create_backend_comm_fn = BackendCommunicationConnect::Create;
      }

      g_free(g_option_backend_init);
    }
  }

  StartEngine(g_option_ibus);
  return 0;
}
