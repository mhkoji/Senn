#include <ibus.h>
#include <glib-object.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstring>
#include <iostream>

#include "senn_ibus/engine.h"


namespace {

IBusEngineClass *g_parent_class = NULL;

senn::ibus::engine::InitCommunicationToBackendServer g_init_comm_fn = NULL;

GObject *SennEngineClassConstructor(
    GType type,
    guint n_construct_properties,
    GObjectConstructParam *construct_properties) {
  return G_OBJECT_CLASS(g_parent_class)->constructor(type,
                                                     n_construct_properties,
                                                     construct_properties);
}

void SennEngineClassDestroy(IBusObject *engine) {
  IBUS_OBJECT_CLASS(g_parent_class)->destroy(engine);
}

void SennEngineClassInit(gpointer klass, gpointer class_data) {
  IBusEngineClass *engine_class = IBUS_ENGINE_CLASS(klass);
  engine_class->process_key_event = senn::ibus::engine::ProcessKeyEvent;

  senn::ibus::engine::EngineClass *senn_engine_class =
    G_TYPE_CHECK_CLASS_CAST(klass,
                            IBUS_TYPE_ENGINE,
                            senn::ibus::engine::EngineClass);
  senn_engine_class->init_comm_fn = g_init_comm_fn;

  g_parent_class = reinterpret_cast<IBusEngineClass*>(
      g_type_class_peek_parent(klass));

  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->constructor = SennEngineClassConstructor;

  IBusObjectClass *ibus_object_class = IBUS_OBJECT_CLASS(klass);
  ibus_object_class->destroy = SennEngineClassDestroy;
}

GType GetType() {
  static GType type = 0;

  static const GTypeInfo type_info = {
    sizeof(senn::ibus::engine::EngineClass),
    NULL,
    NULL,
    SennEngineClassInit,
    NULL,
    NULL,
    sizeof(senn::ibus::engine::Engine),
    0,
    senn::ibus::engine::Init,
  };

  if (type == 0) {
    type = g_type_register_static(IBUS_TYPE_ENGINE,
                                  "IBusSennEngine",
                                  &type_info,
                                  static_cast<GTypeFlags>(0));
    if (type == 0) {
      std::cerr << "g_type_register_static failed";
      exit(1);
    }
  }

  return type;
}

void Disconnected(IBusBus *bus, gpointer user_data) {
  ibus_quit();
}

void StartEngine(bool exec_by_daemon) {
  ibus_init();

  IBusBus *bus = ibus_bus_new();
  g_signal_connect(bus, "disconnected", G_CALLBACK(Disconnected), NULL);

  {
    IBusComponent *component =
      ibus_component_new("org.freedesktop.IBus.Senn",
                         "Senn Component",
                         PACKAGE_VERSION,
                         "MIT",
                         "mhkoji",
                         "",
                         "",
                         "ibus-senn");
    ibus_component_add_engine(component,
                              ibus_engine_desc_new("senn-jp",
                                                   "Senn",
                                                   "Senn Component",
                                                   "ja",
                                                   "MIT",
                                                   "mhkoji",
                                                   "",
                                                   "default"));

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

}  // namespace


namespace {

gboolean g_option_ibus = FALSE;
gchar *g_option_backend_init = NULL;

const GOptionEntry g_option_entries[] = {
  { "ibus", 'i', 0, G_OPTION_ARG_NONE, &g_option_ibus,
    "Component is executed by ibus", NULL },
  { "backend-init", 0, 0, G_OPTION_ARG_STRING, &g_option_backend_init,
    "How to initialize the communication with the backend server", NULL },
  { NULL },
};


class ContextReleaser {
public:
  ContextReleaser(GOptionContext *context) :
    context_(context) {
  }

  ~ContextReleaser() {
    g_option_context_free(context_);
  }

private:
  GOptionContext *context_;
};

} // namespace

int main(gint argc, gchar **argv) {
  // A global variable used during senn::ibus::engine::Init
  g_init_comm_fn = senn::ibus::engine::ServerLaunchInitializer;
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
        g_init_comm_fn = senn::ibus::engine::ServerConnectInitializer;
      }

      g_free(g_option_backend_init);
    }
  }

  StartEngine(g_option_ibus);
  return 0;
}
