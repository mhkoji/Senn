#include <ibus.h>
#include <iostream>

#include "senn_ibus/engine.h"

namespace {

IBusBus *g_bus = NULL;

struct IBusSennEngineClass {
  IBusEngineClass parent;
};

IBusEngineClass *g_parent_class = NULL;

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

  g_parent_class = reinterpret_cast<IBusEngineClass*>(
      g_type_class_peek_parent(klass));

  GObjectClass *object_class = G_OBJECT_CLASS(klass);
  object_class->constructor = MozcEngineClassConstructor;

  IBusObjectClass *ibus_object_class = IBUS_OBJECT_CLASS(klass);
  ibus_object_class->destroy = MozcEngineClassDestroy;
}

void SennEngineInstanceInit(GTypeInstance *instance, gpointer klass) {
  IBusSennEngine *engine =
    reinterpret_cast<senn::ibus::engine::IBusSennEngine*>(instance);
  engine->im = NULL;
}


GType GetType() {
  static GType type = 0;

  static const GTypeInfo type_info = {
    sizeof(IBusSennEngineClass),
    NULL,
    NULL,
    SennEngineClassInit,
    NULL,
    NULL,
    sizeof(IBusSennEngine),
    0,
    SennEngineInstanceInit,
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

}  // namespace

IBusBus *CreateIBusComponent(bool should_request_name) {
  IBusBus *bus = ibus_bus_new();

  g_signal_connect(bus, "disconnected", Disconnected, NULL);

  IBusComponent *component =
    ibus_component_new("org.freedesktop.IBus.Senn",
                       "Senn Component",
                       "0.0.0.0",
                       "MIT",
                       "mhkoji",
                       "https://github.com/mhkoji/Hachee",
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

  IBusFactory *factory = ibus_factory_new(ibus_bus_get_connection(bus));
  ibus_factory_add_engine(factory, "senn-ja", GetType());

  if (should_request_name) {
    ibus_bus_request_name(g_bus, "org.freedesktop.IBus.Senn", 0);
  } else {
    ibus_bus_register_component(bus, component);
  }

  g_object_unref(component);

  return bus;
}

int main(gint argc, gchar **argv) {
  ibus_init();

  g_bus = CreateIBusComponent(false);

  ibus_main();
  return 0;
}
