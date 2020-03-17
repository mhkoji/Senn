#include <fcitx/ime.h>
#include <fcitx/instance.h>
#include <fcitx/context.h>

#include "ui.h"
#include "stateful_im_proxy_ipc.h"
#include "stateful_im_proxy_ipc_server.h"

namespace senn {
namespace fcitx {
namespace iface {

typedef struct _FcitxSennIM {
  FcitxInstance *fcitx;
  senn::fcitx::StatefulIM *im;
  senn::fcitx::StatefulIMProxyIPCServerLauncher *launcher;
} FcitxSennIM;


inline FcitxSennIM *SetupIM(FcitxInstance *fcitx,
                            const std::string &server_program_path) {
  FcitxSennIM *senn = (FcitxSennIM*) fcitx_utils_malloc0(sizeof(FcitxSennIM));
  senn->fcitx = fcitx;

  senn->launcher = new senn::fcitx::StatefulIMProxyIPCServerLauncher(
      server_program_path);
  senn->launcher->Spawn();

  senn->im = new senn::fcitx::StatefulIMProxyIPC(
    std::unique_ptr<senn::ipc::RequesterInterface>(
      new senn::fcitx::ReconnectableStatefulIMRequester(senn->launcher)));

  return senn;
}

inline void DestoryIM(void *arg) {
  FcitxSennIM *senn = (FcitxSennIM *)arg;
  if (senn->im) {
    delete senn->im;
  }
  delete senn->launcher;
  free(senn);
}

inline void ResetIM(void *arg) {
  FcitxSennIM *senn = (FcitxSennIM *)arg;
  FcitxInstance *instance = senn->fcitx;
  senn::fcitx::views::Editing editing_view;
  editing_view.input = "";
  editing_view.cursor_pos = 0;
  senn::fcitx::ui::Show(instance, &editing_view);
}


inline boolean Init(void *arg) {
  FcitxSennIM *senn = (FcitxSennIM *)arg;

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


inline INPUT_RETURN_VALUE DoInput(void *arg,
                                  FcitxKeySym _sym,
                                  uint32_t _state) {
  FcitxSennIM *senn = (FcitxSennIM *)arg;
  FcitxInstance *instance = senn->fcitx;
  FcitxInputState *input = FcitxInstanceGetInputState(instance);

  FcitxKeySym sym = (FcitxKeySym) FcitxInputStateGetKeySym(input);
  uint32_t keycode = FcitxInputStateGetKeyCode(input);
  uint32_t state = FcitxInputStateGetKeyState(input);
  // std::cout << sym << " " << keycode << " " << state << std::endl;

  return senn->im->Transit(sym, keycode, state,
    [&](const senn::fcitx::views::Converting *view) {
      senn::fcitx::ui::Show(instance, view);
    },

    [&](const senn::fcitx::views::Editing *view) {
      senn::fcitx::ui::Show(instance, view);
    });
}

inline INPUT_RETURN_VALUE DoReleaseInput(void *arg,
                                         FcitxKeySym sym,
                                         uint32_t state) {
  return IRV_TO_PROCESS;
}


inline void ReloadConfig(void *arg) {
}


} // iface
} // fcitx
} // senn
