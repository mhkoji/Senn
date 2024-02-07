#include <glib-object.h>
#include <ibus.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#ifdef SENN_IME_ECL
#include "ibus/im/stateful_ime_ecl.h"

const std::string kSennKkcEnginePath = "/usr/lib/senn/kkc/engine";
#elif SENN_IME_SOCKET
#include "ibus/im/stateful_ime_socket.h"
#endif
// #include "ibus/im/stateful_ime_sbcl.h"
// #include "ibus/im/stateful_ime_exec.h"

namespace senn {
namespace ibus_senn {
namespace engine {

class IMEFactory {
public:
  virtual ~IMEFactory() {}

  virtual senn::ibus::im::StatefulIME *CreateIME() = 0;
};

struct EngineClass {
  IBusEngineClass parent;
  senn::ibus::im::StatefulIME *ime;
  IBusPropList *prop_list;
  IBusProperty *prop_input_mode;
};

struct Engine {
  IBusEngine parent;
  senn::ibus::im::StatefulIME *ime;
  IBusPropList *prop_list;
  IBusProperty *prop_input_mode;
};

#define ENGINE(ptr) (reinterpret_cast<Engine *>(ptr))

void Init(GTypeInstance *p, gpointer klass) {
  Engine *engine = ENGINE(p);
  EngineClass *engine_class =
      G_TYPE_CHECK_CLASS_CAST(klass, IBUS_TYPE_ENGINE, EngineClass);
  engine->ime = engine_class->ime;
  engine->prop_list = engine_class->prop_list;
  engine->prop_input_mode = engine_class->prop_input_mode;
}

void ShowCandidateWindow(IBusEngine *engine,
                         const std::vector<std::string> &word_strings,
                         const int index) {
  const gboolean kRound = TRUE;
  const size_t size = word_strings.size();
  IBusLookupTable *table = ibus_lookup_table_new(size, index, TRUE, kRound);

  ibus_lookup_table_set_orientation(table, IBUS_ORIENTATION_VERTICAL);

  for (int i = 0; i < size; ++i) {
    IBusText *text = ibus_text_new_from_string(word_strings[i].c_str());
    // releases text.
    ibus_lookup_table_append_candidate(table, text);
  }

  // releases table
  ibus_engine_update_lookup_table(engine, table, TRUE);
}

void Show(IBusEngine *engine, const senn::fcitx::im::views::Converting *view) {
  IBusText *text = nullptr;
  {
    std::string data;
    std::vector<std::string>::const_iterator it = view->forms.begin();
    for (; it != view->forms.end(); ++it) {
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
    int i = 0, cursor_form_index = view->cursor_form_index;
    std::vector<std::string>::const_iterator it = view->forms.begin();
    for (; it != view->forms.end(); ++it, ++i) {
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

  if (0 < view->cursor_form_candidates.size()) {
    ShowCandidateWindow(engine, view->cursor_form_candidates,
                        view->cursor_form_candidate_index);
  } else {
    ibus_engine_hide_lookup_table(engine);
  }

  ibus_engine_update_preedit_text_with_mode(engine, text, cursor_pos, TRUE,
                                            IBUS_ENGINE_PREEDIT_COMMIT);
}

void Show(IBusEngine *engine, const senn::fcitx::im::views::Editing *view) {
  ibus_engine_hide_lookup_table(engine);

  if (view->committed_input != "") {
    IBusText *committed_text =
        ibus_text_new_from_string(view->committed_input.c_str());
    ibus_engine_commit_text(engine, committed_text);
  }

  if (view->input == "") {
    // This prevents the input console layout from breaking.
    // (The cursor is not shown, for example)
    ibus_engine_hide_preedit_text(engine);
    return;
  }

  IBusText *input_text = ibus_text_new_from_string(view->input.c_str());
  ibus_text_append_attribute(input_text, IBUS_ATTR_TYPE_UNDERLINE,
                             IBUS_ATTR_UNDERLINE_SINGLE, 0,
                             g_utf8_strlen(view->input.c_str(), -1));

  ibus_engine_update_preedit_text_with_mode(
      engine, input_text, g_utf8_strlen(view->input.c_str(), view->cursor_pos),
      TRUE, IBUS_ENGINE_PREEDIT_COMMIT);
}

void UpdatePropInputMode(IBusEngine *p, senn::ibus::im::InputMode mode) {
  Engine *e = ENGINE(p);
  if (mode == senn::ibus::im::InputMode::kHiragana) {
    ibus_property_set_symbol(e->prop_input_mode,
                             ibus_text_new_from_static_string("あ"));
    ibus_property_set_label(
        e->prop_input_mode,
        ibus_text_new_from_static_string("Input Mode (あ)"));
  } else {
    ibus_property_set_symbol(e->prop_input_mode,
                             ibus_text_new_from_static_string("a"));
    ibus_property_set_label(e->prop_input_mode,
                            ibus_text_new_from_static_string("Input Mode (a)"));
  }
  ibus_engine_update_property(p, e->prop_input_mode);
}

class ProcessKeyEventHandler {
public:
  static gboolean execute(IBusEngine *p, guint keyval, guint keycode,
                          guint modifiers) {
    const bool is_key_up = ((modifiers & IBUS_RELEASE_MASK) != 0);
    if (is_key_up) {
      return FALSE;
    }

    if (keyval == IBUS_KEY_Zenkaku_Hankaku) {
      return ToggleInputMode(p);
    }

    return ENGINE(p)->ime->ProcessInput(
        keyval, keycode, modifiers,
        [&](const senn::fcitx::im::views::Converting *view) { Show(p, view); },
        [&](const senn::fcitx::im::views::Editing *view) { Show(p, view); });
  }

private:
  static gboolean ToggleInputMode(IBusEngine *p) {
    ibus_engine_hide_preedit_text(p);
    ibus_engine_hide_lookup_table(p);

    senn::ibus::im::InputMode m = ENGINE(p)->ime->ToggleInputMode();
    UpdatePropInputMode(p, m);
    return true;
  }
};

void CandidateClicked(IBusEngine *p, guint index, guint button, guint state) {
  ENGINE(p)->ime->SelectCandidate(
      index,
      [&](const senn::fcitx::im::views::Converting *view) { Show(p, view); },
      [&](const senn::fcitx::im::views::Editing *view) { Show(p, view); });
}

void FocusIn(IBusEngine *p) {
  Engine *e = ENGINE(p);
  ibus_engine_register_properties(p, e->prop_list);
  // Initial input mode is (a).
  UpdatePropInputMode(p, senn::ibus::im::InputMode::kDirect);
}

void Disable(IBusEngine *p) { ENGINE(p)->ime->ResetIM(); }

} // namespace engine
} // namespace ibus_senn
} // namespace senn

#ifdef SENN_IME_ECL

class EclIMEFactory : public senn::ibus_senn::engine::IMEFactory {
public:
  senn::ibus::im::StatefulIME *CreateIME() {
    return senn::ibus::im::StatefulIMEEcl::Create(kSennKkcEnginePath);
  }

  ~EclIMEFactory() { senn::ibus::im::StatefulIMEEcl::ClShutdown(); }

public:
  static senn::ibus_senn::engine::IMEFactory *Create() {
    senn::ibus::im::StatefulIMEEcl::ClBoot();
    senn::ibus::im::StatefulIMEEcl::EclInitModule();
    return new EclIMEFactory();
  }
};

#elif SENN_IME_SOCKET

class SocketIMEFactory : public senn::ibus_senn::engine::IMEFactory {
public:
  senn::ibus::im::StatefulIME *CreateIME() {
    return senn::ibus::im::StatefulIMESocket::ConnectTo(5678);
  }

  static senn::ibus_senn::engine::IMEFactory *Create() {
    return new SocketIMEFactory();
  }
};

#endif

/*
class SbclIMEFactory : public senn::ibus_senn::engine::IMEFactory {
public:
  senn::ibus::im::StatefulIME *CreateIME() {
    return senn::ibus::im::StatefulIMESbcl::Create();
  }

public:
  static senn::ibus_senn::engine::IMEFactory *Create() {
    senn::ibus::im::StatefulIMESbcl::Init("/usr/lib/senn/libsenn_ibus.core");
    return new SbclIMEFactory();
  }
};
*/

/*
class ExecIMEFactory : public senn::ibus_senn::engine::IMEFactory {
public:
  senn::ibus::im::StatefulIME *CreateIME() {
    return senn::ibus::im::StatefulIMEExec::Create();
  }

public:
  static senn::ibus_senn::engine::IMEFactory *Create() {
    return new ExecIMEFactory();
  }
};
*/

namespace {

IBusEngineClass *g_parent_class = NULL;

// A global variable used during senn::ibus::engine::Init
senn::ibus_senn::engine::IMEFactory *g_ime_factory = nullptr;

GObject *
SennEngineClassConstructor(GType type, guint n_construct_properties,
                           GObjectConstructParam *construct_properties) {
  return G_OBJECT_CLASS(g_parent_class)
      ->constructor(type, n_construct_properties, construct_properties);
}

void SennEngineClassDestroy(IBusObject *p) {
  IBUS_OBJECT_CLASS(g_parent_class)->destroy(p);
}

void SennEngineClassInit(gpointer klass, gpointer class_data) {
  IBusEngineClass *engine_class = IBUS_ENGINE_CLASS(klass);
  // clang-format off
  engine_class->process_key_event =
      senn::ibus_senn::engine::ProcessKeyEventHandler::execute;
  engine_class->candidate_clicked =
      senn::ibus_senn::engine::CandidateClicked;
  engine_class->focus_in =
      senn::ibus_senn::engine::FocusIn;
  engine_class->disable =
      senn::ibus_senn::engine::Disable;
  // clang-format on

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
  // ime
  senn_engine_class->ime = g_ime_factory->CreateIME();
  // prop list
  senn_engine_class->prop_list = ibus_prop_list_new();
  // clang-format off
  // avoid: ibus_engine_register_properties: assertion 'IBUS_IS_PROP_LIST (prop_list)' failed
  // clang-format on
  g_object_ref_sink(senn_engine_class->prop_list);
  // Initial input mode is (a).
  senn_engine_class->prop_input_mode =
      ibus_property_new("InputMode", PROP_TYPE_NORMAL,
                        ibus_text_new_from_string("Input Mode (a)"), nullptr,
                        nullptr, true, true, PROP_STATE_UNCHECKED, nullptr);
  g_object_ref_sink(senn_engine_class->prop_input_mode);
  ibus_prop_list_append(senn_engine_class->prop_list,
                        senn_engine_class->prop_input_mode);
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

const GOptionEntry g_option_entries[] = {
    {"ibus", 'i', 0, G_OPTION_ARG_NONE, &g_option_ibus,
     "Component is executed by ibus", NULL},
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
  GOptionContext *context =
      g_option_context_new("- ibus senn engine component");
  ContextReleaser releaser(context);

  g_option_context_add_main_entries(context, g_option_entries, "ibus-senn");
  GError *error = NULL;
  if (!g_option_context_parse(context, &argc, &argv, &error)) {
    g_print("Option parsing failed: %s\n", error->message);
    std::exit(1);
  }

#ifdef SENN_IME_ECL
  g_ime_factory = EclIMEFactory::Create();
#elif SENN_IME_SOCKET
  g_ime_factory = SocketIMEFactory::Create();
#endif

  StartEngine(g_option_ibus);
  return 0;
}
