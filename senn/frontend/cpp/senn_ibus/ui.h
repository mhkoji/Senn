#pragma once
#include "senn_fcitx/views.h"
#include <ibus.h>

namespace senn {
namespace ibus {
namespace ui {

inline void Show(IBusEngine *engine,
                 const senn::fcitx::views::Converting *converting) {
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

inline void Show(IBusEngine *engine,
                 const senn::fcitx::views::Editing *editing) {

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

} // namespace ui
} // namespace ibus
} // namespace senn
