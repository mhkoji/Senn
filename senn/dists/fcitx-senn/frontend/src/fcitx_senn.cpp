#include <sys/stat.h>
// #include <iostream>

#include <fcitx/ime.h>
#include <fcitx/instance.h>
#include <fcitx/context.h>

#include "senn_fcitx/ui.h"
#include "senn_fcitx/stateful_im_proxy_ipc.h"
#include "senn_fcitx/stateful_im_proxy_ipc_server.h"

namespace {

typedef struct _FcitxSennIM {
  FcitxInstance *fcitx;
  senn::fcitx::StatefulIM *im;
  senn::fcitx::StatefulIMProxyIPCServerLauncher *launcher;

  FcitxUIMenu menu;
} FcitxSennIM;

} // namespace

namespace senn {
namespace fcitx_senn_im {

void ResetIM(void *arg) {
  FcitxSennIM *senn = (FcitxSennIM *)arg;
  FcitxInstance *instance = senn->fcitx;

  senn::fcitx::views::Editing editing_view;
  editing_view.input = "";
  editing_view.cursor_pos = 0;
  senn::fcitx::ui::Show(instance, &editing_view);

  {
    FcitxIM *im = FcitxInstanceGetCurrentIM(instance);
    if (im && strcmp(im->uniqueName, "senn") == 0) {
      senn::fcitx::ui::SetMenuVisibility(instance, true);
    } else {
      senn::fcitx::ui::SetMenuVisibility(instance, false);
    }
  }
}


boolean Init(void *arg) {
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


INPUT_RETURN_VALUE DoInput(void *arg,
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

INPUT_RETURN_VALUE DoReleaseInput(void *arg,
                                  FcitxKeySym sym,
                                  uint32_t state) {
  return IRV_TO_PROCESS;
}


void ReloadConfig(void *arg) {
}

} // fcitx_senn_im
} // senn


static void FcitxSennDestroy(void *arg) {
  FcitxSennIM *senn = (FcitxSennIM *)arg;
  delete senn->im;
  delete senn->launcher;
  free(senn);

  // std::cout << "senn-fcitx: destroyed:"
  //           << " [" << std::hex << arg << "]"
  //           << std::endl;
}

static void* FcitxSennCreate(FcitxInstance *fcitx) {
  FcitxSennIM *senn_im =
    (FcitxSennIM*) fcitx_utils_malloc0(sizeof(FcitxSennIM));

  senn_im->fcitx = fcitx;

  // Stateful IM
  senn_im->launcher = new senn::fcitx::StatefulIMProxyIPCServerLauncher(
      "/usr/lib/senn/server");
  senn_im->launcher->Spawn();
  senn_im->im = new senn::fcitx::StatefulIMProxyIPC(
    std::unique_ptr<senn::ipc::RequesterInterface>(
      new senn::fcitx::ReconnectableStatefulIMRequester(senn_im->launcher)));

  // Menu
  senn::fcitx::ui::SetupMenu(fcitx, &senn_im->menu);
  senn::fcitx::ui::SetMenuVisibility(fcitx, false);

  // Register
  FcitxIMIFace iface;
  memset(&iface, 0, sizeof(FcitxIMIFace));
  iface.Init = senn::fcitx_senn_im::Init;
  iface.ResetIM = senn::fcitx_senn_im::ResetIM;
  iface.DoInput = senn::fcitx_senn_im::DoInput;
  iface.DoReleaseInput = senn::fcitx_senn_im::DoReleaseInput;
  iface.ReloadConfig = senn::fcitx_senn_im::ReloadConfig;
  FcitxInstanceRegisterIMv2(
      fcitx,
      senn_im,
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

  return senn_im;
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
