#include <fcitx/instance.h>

#include <string>

#include "ui.h"

namespace hachee {
namespace fcitx {
namespace ui {

void CommitInput(FcitxInstance *instance,
                 const std::string &in,
                 const int cursor_pos) {
  // 入力を確定
  FcitxInstanceCommitString(
      instance, FcitxInstanceGetCurrentIC(instance), in.c_str());

  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  FcitxMessagesSetMessageCount(client_preedit, 0);

  FcitxInputStateSetClientCursorPos(input, cursor_pos);

  FcitxUIUpdateInputWindow(instance);
};

void UpdateInput(FcitxInstance *instance,
                 const std::string &in,
                 const int cursor_pos) {
  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  // 下線付きの文字列を表示
  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  FcitxMessagesAddMessageAtLast(client_preedit, MSG_INPUT, "%s", in.c_str());

  // カーソルの表示
  FcitxInputStateSetClientCursorPos(input, cursor_pos);

  FcitxUIUpdateInputWindow(instance);
}

} // ui
} // fcitx
} // hachee
