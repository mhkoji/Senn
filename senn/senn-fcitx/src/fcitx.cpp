#include <fcitx/instance.h>
#include <fcitx/ime.h>
#include <fcitx/context.h>
#include <sys/stat.h>
#include <string>
#include <iostream>

#include "ui.h"
#include "ipc_stateful_im_proxy.h"
#include "ipc_stateful_im_proxy_server.h"

const std::string SOCKET_PATH = "/tmp/senn-server-socket";


typedef struct _FcitxSenn {
  FcitxInstance *fcitx;
  senn::fcitx::StatefulIMProxy *im;
} FcitxSenn;


static void FcitxSennDestroy(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  delete senn->im;
  free(senn);
}

static boolean FcitxSennInit(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  if (!senn->im) {
    senn->im = senn::fcitx::IPCStatefulIMProxy::Create(
        senn::ipc::Connection::ConnectAbstractTo(SOCKET_PATH));
  }

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

  return true;
}

static void FcitxSennReset(void *arg) {
}

INPUT_RETURN_VALUE FcitxSennDoInput(void *arg,
                                    FcitxKeySym _sym,
                                    uint32_t _state) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  FcitxInstance *instance = senn->fcitx;
  FcitxInputState *input = FcitxInstanceGetInputState(instance);

  FcitxKeySym sym = (FcitxKeySym) FcitxInputStateGetKeySym(input);
  uint32_t keycode = FcitxInputStateGetKeyCode(input);
  uint32_t state = FcitxInputStateGetKeyState(input);
  // std::cout << sym << " " << keycode << " " << state << std::endl;

  return senn->im->Input(sym, keycode, state,
    [&](const senn::fcitx::views::Committed *view) {
      senn::fcitx::ui::Draw(instance, view);
    },

    [&](const senn::fcitx::views::Converting *view) {
      senn::fcitx::ui::Draw(instance, view);
    },

    [&](const senn::fcitx::views::Editing *view) {
      senn::fcitx::ui::Draw(instance, view);
    });
}

INPUT_RETURN_VALUE FcitxSennDoReleaseInput(void *arg,
                                           FcitxKeySym sym,
                                           uint32_t state) {
  return IRV_TO_PROCESS;
}

void FcitxSennReloadConfig(void *arg) {
}

static void* FcitxSennCreate(FcitxInstance *fcitx) {
  FcitxSenn *senn = (FcitxSenn*) fcitx_utils_malloc0(
      sizeof(FcitxSenn)
  );
  senn->fcitx = fcitx;
  senn->im = nullptr;

  FcitxIMIFace iface;
  memset(&iface, 0, sizeof(FcitxIMIFace));
  iface.Init = FcitxSennInit;
  iface.ResetIM = FcitxSennReset;
  iface.DoInput = FcitxSennDoInput;
  iface.DoReleaseInput = FcitxSennDoReleaseInput;
  iface.ReloadConfig = FcitxSennReloadConfig;

  senn::fcitx::StartIPCServer(SOCKET_PATH);

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
