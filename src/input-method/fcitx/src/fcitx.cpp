#include <fcitx/instance.h>
#include <fcitx/ime.h>
#include <fcitx/context.h>
#include <string>

#include "client.h"

typedef struct _FcitxHachee {
  FcitxInstance *fcitx;
  hachee::fcitx::Client *client;
} FcitxHachee;


static void FcitxHacheeDestroy(void *arg) {
  FcitxHachee *hachee = (FcitxHachee *)arg;
  delete hachee->client;
  free(hachee);
}

static boolean FcitxHacheeInit(void *arg) {
  FcitxHachee *hachee = (FcitxHachee *)arg;
  boolean flag = true;
  FcitxInstanceSetContext(hachee->fcitx,
                          CONTEXT_IM_KEYBOARD_LAYOUT,
                          "jp");
  FcitxInstanceSetContext(hachee->fcitx,
                          CONTEXT_DISABLE_AUTO_FIRST_CANDIDATE_HIGHTLIGHT,
                          &flag);
  FcitxInstanceSetContext(hachee->fcitx,
                          CONTEXT_DISABLE_AUTOENG,
                          &flag);
  FcitxInstanceSetContext(hachee->fcitx,
                          CONTEXT_DISABLE_QUICKPHRASE,
                          &flag);

  return true;
}

static void FcitxHacheeReset(void *arg) {
}

INPUT_RETURN_VALUE FcitxHacheeDoReleaseInput(void *arg,
                                             FcitxKeySym sym,
                                             uint32_t state) {
  return IRV_TO_PROCESS;
}

INPUT_RETURN_VALUE FcitxHacheeDoInput(void *arg,
                                      FcitxKeySym _sym,
                                      uint32_t _state) {
  FcitxHachee *hachee = (FcitxHachee *)arg;
  FcitxInputState* input = FcitxInstanceGetInputState(hachee->fcitx);
  FcitxKeySym sym = (FcitxKeySym) FcitxInputStateGetKeySym(input);
  uint32_t keycode = FcitxInputStateGetKeyCode(input);
  uint32_t state = FcitxInputStateGetKeyState(input);

  std::string result_string;
  hachee->client->ProcessKey(sym, keycode, state, &result_string);

  FcitxInstanceCommitString(
      hachee->fcitx,
      FcitxInstanceGetCurrentIC(hachee->fcitx),
      result_string.c_str()
  );

  return IRV_DISPLAY_CANDWORDS;
}

void FcitxHacheeReloadConfig(void *arg) {
}

static void* FcitxHacheeCreate(FcitxInstance *fcitx) {
  FcitxHachee *hachee = (FcitxHachee*) fcitx_utils_malloc0(
      sizeof(FcitxHachee)
  );
  hachee->fcitx = fcitx;
  hachee->client = new hachee::fcitx::Client();

  FcitxIMIFace iface;
  memset(&iface, 0, sizeof(FcitxIMIFace));
  iface.Init = FcitxHacheeInit;
  iface.ResetIM = FcitxHacheeReset;
  iface.DoInput = FcitxHacheeDoInput;
  iface.DoReleaseInput = FcitxHacheeDoReleaseInput;
  iface.ReloadConfig = FcitxHacheeReloadConfig;

  FcitxInstanceRegisterIMv2(
      fcitx,
      hachee,
      "hachee",
      "Hachee",
      "hachee",
      iface,
      10,
      "ja"
  );

  return hachee;
}

extern "C" {

FCITX_EXPORT_API
FcitxIMClass ime = {
  FcitxHacheeCreate,
  FcitxHacheeDestroy
};

FCITX_EXPORT_API
int ABI_VERSION = FCITX_ABI_VERSION;

}
