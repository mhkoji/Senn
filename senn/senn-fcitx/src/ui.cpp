#include <fcitx/candidate.h>
#include <fcitx/instance.h>
#include <string>
#include <sstream>

#include "ui.h"

namespace senn {
namespace fcitx {
namespace ui {

void Committed(FcitxInstance *instance,
               const senn::fcitx::states::Committed *committed) {
  // 入力を確定
  FcitxInputContext *ic = FcitxInstanceGetCurrentIC(instance);
  FcitxInstanceCommitString(instance, ic, committed->input.c_str());

  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxUIUpdateInputWindow(instance);
};

void Converting(FcitxInstance *instance,
                const senn::fcitx::states::Converting *converting) {
  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  {
    int i = 0, cursor_pos = converting->cursor_pos;
    std::vector<std::string>::const_iterator it = converting->forms.begin();
    for (; it != converting->forms.end(); ++it, ++i) {
      FcitxMessageType type = i == cursor_pos ?
        (FcitxMessageType) (MSG_HIGHLIGHT | MSG_CANDIATE_CURSOR) :
        (FcitxMessageType) (MSG_INPUT);
      FcitxMessagesAddMessageAtLast(client_preedit, type, "%s", it->c_str());
    }
  }

  FcitxUIUpdateInputWindow(instance);
}

void Editing(FcitxInstance *instance,
             const senn::fcitx::states::Editing *editing) {
  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  // 下線付きの文字列を表示
  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  FcitxMessagesAddMessageAtLast(client_preedit,
                                MSG_INPUT,
                                "%s",
                                editing->input.c_str());

  // カーソルの表示
  FcitxInputStateSetClientCursorPos(input, editing->cursor_pos);

  FcitxUIUpdateInputWindow(instance);
}

} // ui
} // fcitx
} // senn
