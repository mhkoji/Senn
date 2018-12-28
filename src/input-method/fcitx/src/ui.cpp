#include <fcitx/instance.h>

#include <string>

#include "ui.h"

namespace hachee {
namespace fcitx {
namespace ui {

void CommitInput(FcitxInstance *instance,
                 const std::string &msg) {
  // 入力を確定
  FcitxInstanceCommitString(
      instance, FcitxInstanceGetCurrentIC(instance), msg.c_str());

  // 表示している文字列を削除
  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  FcitxMessagesSetMessageCount(client_preedit, 0);

  FcitxUIUpdateInputWindow(instance);
};

void UpdateInput(FcitxInstance *instance,
                 const std::string &msg, const int cursor_pos) {
  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  // 表示している文字列を削除
  FcitxMessagesSetMessageCount(
     client_preedit, 0);

  // 下線付きの文字列を表示
  FcitxMessagesAddMessageAtLast(
      client_preedit, MSG_INPUT, "%s", msg.c_str());

  // カーソルの表示
  FcitxInputStateSetClientCursorPos(input, cursor_pos);

  FcitxUIUpdateInputWindow(instance);
}

} // ui
} // fcitx
} // hachee
