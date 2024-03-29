#include <sys/stat.h>
// #include <iostream>

#include <fcitx/candidate.h>
#include <fcitx/context.h>
#include <fcitx/hook.h>
#include <fcitx/ime.h>
#include <fcitx/instance.h>

#ifdef SENN_IME_ECL
#include "fcitx/im/stateful_ime_ecl.h"

const char *kECLDIR = "/usr/lib/senn/fcitx/ecl/lib/ecl-21.2.1/";
const std::string kKkcEnginePath = "/usr/lib/senn/fcitx/kkc-engine";
#elif SENN_IME_SOCKET
#include "fcitx/im/stateful_ime_socket.h"
#endif
// #include "fcitx/im/stateful_ime_sbcl.h"

namespace {

typedef struct _FcitxSenn {
  FcitxInstance *fcitx;
  senn::fcitx::im::StatefulIME *ime;
} FcitxSenn;

} // namespace

namespace senn {
namespace fcitx_senn {
namespace im {

INPUT_RETURN_VALUE
CandidateWordCallback(void *arg, FcitxCandidateWord *word);

void ShowCandidateWordList(FcitxSenn *senn, FcitxInputState *input,
                           const std::vector<std::string> &word_strings,
                           const int index) {
  FcitxCandidateWordList *word_list = FcitxInputStateGetCandidateList(input);
  FcitxCandidateWordReset(word_list);
  FcitxCandidateWordSetLayoutHint(word_list, CLH_Vertical);
  std::vector<std::string>::const_iterator it = word_strings.begin();
  for (int i = 0; it != word_strings.end(); ++it, ++i) {
    FcitxCandidateWord word;
    int *p = fcitx_utils_new(int);
    *p = i;
    word.callback = CandidateWordCallback;
    word.extraType = MSG_OTHER;
    word.owner = senn;
    word.priv = (void *)p;
    word.strExtra = NULL;
    word.strWord = strdup(it->c_str());
    word.wordType = (i == index) ? MSG_CANDIATE_CURSOR : MSG_OTHER;
    FcitxCandidateWordAppend(word_list, &word);
  }

  if (0 <= index && index < word_strings.size()) {
    // Set page by word index
    FcitxCandidateWordSetFocus(word_list, index);

    FcitxMessages *aux = FcitxInputStateGetAuxUp(input);
    FcitxMessagesSetMessageCount(aux, 0);
    FcitxMessagesAddMessageAtLast(aux, MSG_TIPS, "(%d / %d)", index + 1,
                                  word_strings.size());
  }
}

void Show(FcitxSenn *senn, const senn::fcitx::im::views::Converting *view) {
  FcitxInstance *instance = senn->fcitx;

  // 表示している文字列, candidate windowを削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxInputState *input = FcitxInstanceGetInputState(instance);

  // 下線付きの文字列を表示
  {
    FcitxInputContext *ic = FcitxInstanceGetCurrentIC(instance);
    FcitxMessages *preedit = FcitxInputStateGetPreedit(input);
    FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
    boolean support_preedit = FcitxInstanceICSupportPreedit(instance, ic);

    int i = 0, cursor_form_index = view->cursor_form_index;
    std::vector<std::string>::const_iterator it = view->forms.begin();
    for (; it != view->forms.end(); ++it, ++i) {
      FcitxMessageType type =
          (i == cursor_form_index)
              ? (FcitxMessageType)(MSG_HIGHLIGHT | MSG_CANDIATE_CURSOR)
              : (FcitxMessageType)(MSG_INPUT);
      if (!support_preedit) {
        FcitxMessagesAddMessageAtLast(preedit, type, "%s", it->c_str());
      }
      FcitxMessagesAddMessageAtLast(client_preedit, type, "%s", it->c_str());
    }
  }

  // candidate windowを表示
  if (0 < view->cursor_form_candidates.size()) {
    ShowCandidateWordList(senn, input, view->cursor_form_candidates,
                          view->cursor_form_candidate_index);
  }
}

void Show(FcitxSenn *senn, const senn::fcitx::im::views::Editing *view) {
  FcitxInstance *instance = senn->fcitx;

  // 表示している文字列, candidate windowを削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxInputContext *ic = FcitxInstanceGetCurrentIC(instance);

  // 入力を確定
  if (view->committed_input != "") {
    FcitxInstanceCommitString(instance, ic, view->committed_input.c_str());
  }

  // 空文字列を描画するとカーソル移動できなくなるため、
  // 空文字列の場合は描画しない
  if (view->input == "") {
    return;
  }

  // 下線付きの文字列を表示
  {
    FcitxMessages *preedit = FcitxInputStateGetPreedit(input);
    FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
    boolean support_preedit = FcitxInstanceICSupportPreedit(instance, ic);

    if (!support_preedit) {
      FcitxMessagesAddMessageAtLast(preedit, MSG_INPUT, "%s",
                                    view->input.c_str());
    }
    FcitxMessagesAddMessageAtLast(client_preedit, MSG_INPUT, "%s",
                                  view->input.c_str());
  }

  // candidate windowを表示
  if (0 < view->predictions.size()) {
    ShowCandidateWordList(senn, input, view->predictions,
                          view->prediction_index);
  }

  // カーソルの表示
  FcitxInputStateSetClientCursorPos(input, view->cursor_pos);
}

INPUT_RETURN_VALUE
CandidateWordCallback(void *arg, FcitxCandidateWord *word) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  int index = *(int *)word->priv;
  bool consumed = senn->ime->SelectCandidate(
      index,
      [&](const fcitx::im::views::Converting *view) { Show(senn, view); },
      [&](const fcitx::im::views::Editing *view) { Show(senn, view); });
  return consumed ? IRV_DISPLAY_CANDWORDS : IRV_TO_PROCESS;
}

void ResetInput(void *arg) {}

void ResetIM(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  senn->ime->ResetIM();

  {
    senn::fcitx::im::views::Editing view;
    view.input = "";
    view.cursor_pos = 0;
    Show(senn, &view);
  }
}

boolean Init(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  boolean flag = true;
  FcitxInstanceSetContext(senn->fcitx, CONTEXT_IM_KEYBOARD_LAYOUT, "jp");
  FcitxInstanceSetContext(
      senn->fcitx, CONTEXT_DISABLE_AUTO_FIRST_CANDIDATE_HIGHTLIGHT, &flag);
  FcitxInstanceSetContext(senn->fcitx, CONTEXT_DISABLE_AUTOENG, &flag);
  FcitxInstanceSetContext(senn->fcitx, CONTEXT_DISABLE_QUICKPHRASE, &flag);

  // std::cout << "senn-fcitx: initialized:"
  //           << " [" << std::hex << arg << "]"
  //           << std::endl;

  return true;
}

INPUT_RETURN_VALUE DoInput(void *arg, FcitxKeySym _sym, uint32_t _state) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  FcitxInstance *instance = senn->fcitx;
  FcitxInputState *input = FcitxInstanceGetInputState(instance);

  uint32_t sym = FcitxInputStateGetKeySym(input);
  uint32_t keycode = FcitxInputStateGetKeyCode(input);
  uint32_t state = FcitxInputStateGetKeyState(input);
  // std::cout << sym << " " << keycode << " " << state << std::endl;

  boolean has_view = false;
  boolean consumed = senn->ime->ProcessInput(
      sym, keycode, state,
      [&](const fcitx::im::views::Converting *view) {
        has_view = true;
        Show(senn, view);
      },
      [&](const fcitx::im::views::Editing *view) {
        has_view = true;
        Show(senn, view);
      });

  if (consumed) {
    return has_view ? IRV_DISPLAY_CANDWORDS : IRV_DO_NOTHING;
  }
  return IRV_TO_PROCESS;
}

INPUT_RETURN_VALUE DoReleaseInput(void *arg, FcitxKeySym sym, uint32_t state) {
  return IRV_TO_PROCESS;
}

void ReloadConfig(void *arg) {}

} // namespace im
} // namespace fcitx_senn
} // namespace senn

static void FcitxSennDestroy(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  delete senn->ime;

#ifdef SENN_IME_ECL
  senn::fcitx::im::StatefulIMEEcl::ClShutdown();
#endif

  free(senn);

  // std::cout << "senn-fcitx: destroyed:"
  //           << " [" << std::hex << arg << "]"
  //           << std::endl;
}

static void *FcitxSennCreate(FcitxInstance *fcitx) {
  FcitxSenn *senn = (FcitxSenn *)fcitx_utils_malloc0(sizeof(FcitxSenn));

  senn->fcitx = fcitx;

  // StatefulIME
#ifdef SENN_IME_ECL
  setenv("ECLDIR", kECLDIR, 1);
  senn::fcitx::im::StatefulIMEEcl::ClBoot();
  senn::fcitx::im::StatefulIMEEcl::EclInitModule();
  senn->ime = senn::fcitx::im::StatefulIMEEcl::Create(kKkcEnginePath);
#elif SENN_IME_SOCKET
  senn->ime = senn::fcitx::im::StatefulIMESocket::SpawnAndCreate(
      "/usr/lib/senn/server");
#endif
  /*
  senn::fcitx::im::StatefulIMESbcl::Init("/usr/lib/senn/libsennfcitx.core");
  senn->ime = senn::fcitx::im::StatefulIMESbcl::Create()
  */

  FcitxIMEventHook hk;
  hk.arg = senn;
  hk.func = senn::fcitx_senn::im::ResetInput;
  FcitxInstanceRegisterResetInputHook(fcitx, hk);

  // Register
  FcitxIMIFace iface;
  memset(&iface, 0, sizeof(FcitxIMIFace));
  iface.Init = senn::fcitx_senn::im::Init;
  iface.ResetIM = senn::fcitx_senn::im::ResetIM;
  iface.DoInput = senn::fcitx_senn::im::DoInput;
  iface.DoReleaseInput = senn::fcitx_senn::im::DoReleaseInput;
  iface.ReloadConfig = senn::fcitx_senn::im::ReloadConfig;
  FcitxInstanceRegisterIMv2(fcitx, senn, "senn", "Senn", "senn", iface, 10,
                            "ja");

  // std::cout << "senn-fcitx: created:"
  //           << " [" << std::hex << senn << "]"
  //           << std::endl;

  return senn;
}

extern "C" {

FCITX_EXPORT_API
FcitxIMClass ime = {FcitxSennCreate, FcitxSennDestroy};

FCITX_EXPORT_API
int ABI_VERSION = FCITX_ABI_VERSION;

} // extern "C"
