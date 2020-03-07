#pragma once
#include <ibus.h>
#include "senn_fcitx/views.h"

namespace senn {
namespace ibus {
namespace ui {

inline void Show(const senn::fcitx::views::Converting*) {
  // TODO
}

inline void Show(IBusEngine *engine,
                 const senn::fcitx::views::Editing* editing) {
  IBusText *input = ibus_text_new_from_string(editing->input.c_str());
  ibus_text_append_attribute(
      input,
      IBUS_ATTR_TYPE_UNDERLINE,
      IBUS_ATTR_UNDERLINE_SINGLE,
      0,
      editing->input.size());
  ibus_engine_update_preedit_text_with_mode(
      engine,
      input,
      editing->cursor_pos,
      TRUE,
      IBUS_ENGINE_PREEDIT_COMMIT);
}


} // ui
} // ibus
} // senn
