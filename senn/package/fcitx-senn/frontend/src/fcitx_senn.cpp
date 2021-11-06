#include <sys/stat.h>
// #include <iostream>

#include <fcitx/ime.h>
#include <fcitx/hook.h>
#include <fcitx/instance.h>
#include <fcitx/candidate.h>
#include <fcitx/context.h>

#include "process/process.h"
#include "senn_fcitx/im/stateful_ime_proxy_ipc.h"
#include "senn_fcitx/im/stateful_ime_proxy_ipc_server.h"

namespace {

typedef struct _FcitxSenn {
  FcitxInstance *fcitx;
  FcitxUIMenu menu;

  senn::fcitx::im::StatefulIMEProxyIPC *ime;
  senn::fcitx::im::StatefulIMEProxyIPCServerLauncher *launcher;
} FcitxSenn;

} // namespace

namespace senn {
namespace fcitx_senn {
namespace ui {
namespace menu {

const char* GetIconName(void* arg) {
  return "";
}

void Update(FcitxUIMenu *menu) {}

boolean Action(FcitxUIMenu *menu, int index) {
  return senn::process::Spawn("/usr/lib/senn/menu-about");
}

void SetVisibility(FcitxInstance *fcitx, boolean vis) {
  FcitxUISetStatusVisable(fcitx, "senn-menu", vis);
}

void Setup(FcitxInstance *fcitx, FcitxUIMenu *menu) {
  FcitxUIRegisterComplexStatus(
      fcitx,
      NULL,
      "senn-menu",
      "メニュー",
      "メニュー",
      NULL,
      GetIconName);

  FcitxMenuInit(menu);
  menu->name = strdup("メニュー");
  menu->candStatusBind = strdup("senn-menu");
  menu->UpdateMenu = Update;
  menu->MenuAction = Action;
  menu->priv = nullptr;
  menu->isSubMenu = false;
  FcitxMenuAddMenuItem(menu, "Senn について", MENUTYPE_SIMPLE, NULL);
  FcitxUIRegisterMenu(fcitx, menu);

  SetVisibility(fcitx, false);
}

void Destory(FcitxInstance *fcitx, FcitxUIMenu *menu) {
  FcitxUIUnRegisterMenu(fcitx, menu);
  fcitx_utils_free(menu->name);
  fcitx_utils_free(menu->candStatusBind);
  FcitxMenuFinalize(menu);
}

} // menu

namespace input {

INPUT_RETURN_VALUE
CandidateWordCallback(void* arg, FcitxCandidateWord* word) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  int index = *(int*)word->priv;
  bool selected = senn->ime->SelectCandidate(index);
  return selected ? IRV_DISPLAY_CANDWORDS : IRV_TO_PROCESS;
}

void ShowCandidateWordList(
    FcitxSenn *senn,
    FcitxInputState *input,
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
    word.priv = (void*) p;
    word.strExtra = NULL;
    word.strWord = strdup(it->c_str());
    word.wordType = (i == index) ? MSG_CANDIATE_CURSOR : MSG_OTHER;
    FcitxCandidateWordAppend(word_list, &word);
  }

  if (0 <= index && index < word_strings.size()) {
    // Set page by word index
    FcitxCandidateWordSetFocus(word_list, index);

    FcitxMessages* aux = FcitxInputStateGetAuxUp(input);
    FcitxMessagesSetMessageCount(aux, 0);
    FcitxMessagesAddMessageAtLast(aux, MSG_TIPS, "(%d / %d)",
                                  index + 1, word_strings.size());
  }
}

void Show(FcitxSenn *senn,
          const senn::fcitx::im::views::Converting *converting) {
  FcitxInstance *instance = senn->fcitx;
  
  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxInputContext *ic = FcitxInstanceGetCurrentIC(instance);
  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *preedit = FcitxInputStateGetPreedit(input);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  boolean support_preedit = FcitxInstanceICSupportPreedit(instance, ic);

  // 下線付きの文字列を表示
  {
    int i = 0, cursor_form_index = converting->cursor_form_index;
    std::vector<std::string>::const_iterator it = converting->forms.begin();
    for (; it != converting->forms.end(); ++it, ++i) {
      FcitxMessageType type = (i == cursor_form_index) ?
        (FcitxMessageType) (MSG_HIGHLIGHT | MSG_CANDIATE_CURSOR) :
        (FcitxMessageType) (MSG_INPUT);
      if (!support_preedit) {
        FcitxMessagesAddMessageAtLast(preedit, type, "%s", it->c_str());
      }
      FcitxMessagesAddMessageAtLast(client_preedit, type, "%s", it->c_str());
    }
  }

  if (0 < converting->cursor_form_candidates.size()) {
    ShowCandidateWordList(senn, input,
                          converting->cursor_form_candidates,
                          converting->cursor_form_candidate_index);
  }

  FcitxUIUpdateInputWindow(instance);
}

void Show(FcitxSenn *senn,
          const senn::fcitx::im::views::Editing *editing) {
  FcitxInstance *instance = senn->fcitx;
  FcitxInputContext *ic = FcitxInstanceGetCurrentIC(instance);

  if (editing->committed_input != "") {
    // 入力を確定
    FcitxInstanceCommitString(instance, ic, editing->committed_input.c_str());
  }

  // 表示している文字列を削除
  FcitxInstanceCleanInputWindow(instance);

  FcitxInputState *input = FcitxInstanceGetInputState(instance);
  FcitxMessages *preedit = FcitxInputStateGetPreedit(input);
  FcitxMessages *client_preedit = FcitxInputStateGetClientPreedit(input);
  boolean support_preedit = FcitxInstanceICSupportPreedit(instance, ic);

  // 下線付きの文字列を表示
  if (!support_preedit) {
    FcitxMessagesAddMessageAtLast(
        preedit, MSG_INPUT, "%s", editing->input.c_str());
  }
  FcitxMessagesAddMessageAtLast(
      client_preedit, MSG_INPUT, "%s", editing->input.c_str());

  if (0 < editing->predictions.size()) {
    ShowCandidateWordList(senn, input,
                          editing->predictions,
                          editing->prediction_index);
  }

  // カーソルの表示
  if (!support_preedit) {
    FcitxInputStateSetCursorPos(input, editing->cursor_pos);
  }
  FcitxInputStateSetClientCursorPos(input, editing->cursor_pos);

  FcitxUIUpdateInputWindow(instance);
}

} // input
} // ui

static void ResetInput(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  FcitxInstance *instance = senn->fcitx;

  FcitxIM *im = FcitxInstanceGetCurrentIM(instance);
  if (im && strcmp(im->uniqueName, "senn") == 0) {
    ui::menu::SetVisibility(instance, true);
  } else {
    ui::menu::SetVisibility(instance, false);
  }
}


void ResetIM(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  senn->ime->ResetIM();

  fcitx::im::views::Editing editing_view;
  editing_view.input = "";
  editing_view.cursor_pos = 0;
  fcitx_senn::ui::input::Show(senn, &editing_view);
}


boolean Init(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  boolean flag = true;
  FcitxInstanceSetContext(senn->fcitx,
                          CONTEXT_IM_KEYBOARD_LAYOUT,
                          "jp");
  FcitxInstanceSetContext(senn->fcitx,
                          CONTEXT_DISABLE_AUTO_FIRST_CANDIDATE_HIGHTLIGHT,
                          &flag);
  FcitxInstanceSetContext(senn->fcitx,
                          CONTEXT_DISABLE_AUTOENG,
                          &flag);
  FcitxInstanceSetContext(senn->fcitx,
                          CONTEXT_DISABLE_QUICKPHRASE,
                          &flag);

  // std::cout << "senn-fcitx: initialized:"
  //           << " [" << std::hex << arg << "]"
  //           << std::endl;

  return true;
}


INPUT_RETURN_VALUE DoInput(void *arg,
                           FcitxKeySym _sym,
                           uint32_t _state) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  FcitxInstance *instance = senn->fcitx;
  FcitxInputState *input = FcitxInstanceGetInputState(instance);

  FcitxKeySym sym = (FcitxKeySym) FcitxInputStateGetKeySym(input);
  uint32_t keycode = FcitxInputStateGetKeyCode(input);
  uint32_t state = FcitxInputStateGetKeyState(input);
  // std::cout << sym << " " << keycode << " " << state << std::endl;

  return senn->ime->ProcessInput(sym, keycode, state,
    [&](const fcitx::im::views::Converting *view) {
      fcitx_senn::ui::input::Show(senn, view);
    },

    [&](const fcitx::im::views::Editing *view) {
      fcitx_senn::ui::input::Show(senn, view);
    });
}

INPUT_RETURN_VALUE DoReleaseInput(void *arg,
                                  FcitxKeySym sym,
                                  uint32_t state) {
  return IRV_TO_PROCESS;
}


void ReloadConfig(void *arg) {
}

} // fcitx_senn
} // senn


static void FcitxSennDestroy(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  delete senn->ime;
  delete senn->launcher;

  senn::fcitx_senn::ui::menu::Destory(senn->fcitx, &senn->menu);

  free(senn);

  // std::cout << "senn-fcitx: destroyed:"
  //           << " [" << std::hex << arg << "]"
  //           << std::endl;
}

static void* FcitxSennCreate(FcitxInstance *fcitx) {
  FcitxSenn *senn =
    (FcitxSenn*) fcitx_utils_malloc0(sizeof(FcitxSenn));

  senn->fcitx = fcitx;

  // StatefulIME
  senn->launcher =
    new senn::fcitx::im::StatefulIMEProxyIPCServerLauncher(
      "/usr/lib/senn/server");
  senn->launcher->Spawn();

  senn->ime =
    new senn::fcitx::im::StatefulIMEProxyIPC(
      std::unique_ptr<senn::ipc::RequesterInterface>(
        new senn::fcitx::im::ReconnectableStatefulIMERequester(
          senn->launcher)));

  FcitxIMEventHook hk;
  hk.arg = senn;
  hk.func = senn::fcitx_senn::ResetInput;
  FcitxInstanceRegisterResetInputHook(fcitx, hk);
  
  // Menu
  senn::fcitx_senn::ui::menu::Setup(senn->fcitx, &senn->menu);

  // Register
  FcitxIMIFace iface;
  memset(&iface, 0, sizeof(FcitxIMIFace));
  iface.Init           = senn::fcitx_senn::Init;
  iface.ResetIM        = senn::fcitx_senn::ResetIM;
  iface.DoInput        = senn::fcitx_senn::DoInput;
  iface.DoReleaseInput = senn::fcitx_senn::DoReleaseInput;
  iface.ReloadConfig   = senn::fcitx_senn::ReloadConfig;
  FcitxInstanceRegisterIMv2(
      fcitx,
      senn,
      "senn",
      "Senn",
      "senn",
      iface,
      10,
      "ja"
  );

  // std::cout << "senn-fcitx: created:"
  //           << " [" << std::hex << senn << "]"
  //           << std::endl;

  return senn;
}

extern "C" {

FCITX_EXPORT_API
FcitxIMClass ime = {
  FcitxSennCreate,
  FcitxSennDestroy
};

FCITX_EXPORT_API
int ABI_VERSION = FCITX_ABI_VERSION;

} // extern "C"
