#include <fcitx/candidate.h>
#include <fcitx/instance.h>
#include <string>
#include <sstream>

#include "ui.h"

namespace senn {
namespace fcitx {
namespace ui {

void Draw(FcitxInstance *instance,
          const senn::fcitx::views::Committed *committed) {
  // 入力を確定
  FcitxInputContext *ic = FcitxInstanceGetCurrentIC(instance);
  FcitxInstanceCommitString(instance, ic, committed->input.c_str());

  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxUIUpdateInputWindow(instance);
};


INPUT_RETURN_VALUE
get_candidate(void* arg, FcitxCandidateWord* word) {
  return IRV_DO_NOTHING;
}

void Draw(FcitxInstance *instance,
          const senn::fcitx::views::Converting *converting) {
  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  {
    int i = 0, cursor_form_index = converting->cursor_form_index;
    std::vector<std::string>::const_iterator it = converting->forms.begin();
    for (; it != converting->forms.end(); ++it, ++i) {
      FcitxMessageType type = (i == cursor_form_index) ?
        (FcitxMessageType) (MSG_HIGHLIGHT | MSG_CANDIATE_CURSOR) :
        (FcitxMessageType) (MSG_INPUT);
      FcitxMessagesAddMessageAtLast(client_preedit, type, "%s", it->c_str());
    }
  }

  if (0 < converting->cursor_form_candidates.size()) {
    FcitxCandidateWordList *word_list =
        FcitxInputStateGetCandidateList(input);
    FcitxCandidateWordReset(word_list);
    FcitxCandidateWordSetLayoutHint(word_list, CLH_Vertical);
    std::vector<std::string>::const_iterator it =
        converting->cursor_form_candidates.begin();
    for (int i = 0; it != converting->cursor_form_candidates.end();
         ++it, ++i) {
      FcitxCandidateWord word;
      int *p = fcitx_utils_new(int);
      *p = i;
      word.callback = get_candidate;
      word.extraType = MSG_OTHER;
      word.owner = instance;
      word.priv = (void*) p;
      word.strExtra = NULL;
      word.strWord = strdup(it->c_str());
      word.wordType = (i == converting->cursor_form_candidate_index) ?
        MSG_CANDIATE_CURSOR : MSG_OTHER;
      FcitxCandidateWordAppend(word_list, &word);
    }
    // Set page by word index
    FcitxCandidateWordSetFocus(word_list,
                               converting->cursor_form_candidate_index);

    FcitxMessages* aux = FcitxInputStateGetAuxUp(input);
    FcitxMessagesSetMessageCount(aux, 0);
    FcitxMessagesAddMessageAtLast(aux, MSG_TIPS, "(%d / %d)",
                                  converting->cursor_form_candidate_index + 1,
                                  converting->cursor_form_candidates.size());
  }

  FcitxUIUpdateInputWindow(instance);
}

void Draw(FcitxInstance *instance,
          const senn::fcitx::views::Editing *editing) {
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
