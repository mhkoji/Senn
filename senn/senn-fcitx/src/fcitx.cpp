#include <fcitx/instance.h>
#include <fcitx/ime.h>
#include <fcitx/context.h>
#include <string>
#include <iostream>

#include "ui.h"
#include "ipc_client.h"
#include "ipc_server.h"

const std::string SOCKET_NAME = "/tmp/senn.sock";


typedef struct _FcitxSenn {
  FcitxInstance *fcitx;
  senn::fcitx::Client *client;
} FcitxSenn;


static void FcitxSennDestroy(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;
  delete senn->client;
  free(senn);
}

static boolean FcitxSennInit(void *arg) {
  FcitxSenn *senn = (FcitxSenn *)arg;

  if (!senn->client) {
    senn->client = senn::fcitx::IPCClient::Create(SOCKET_NAME);
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
  // uint32_t keycode = FcitxInputStateGetKeyCode(input);
  // uint32_t state = FcitxInputStateGetKeyState(input);
  // std::cout << sym << " " << keycode << " " << state << std::endl;

  return senn->client->TransitByInput(
    sym,

    [&](const senn::fcitx::states::Committed *state) {
      senn::fcitx::ui::Committed(instance, state);
      // 何らかの文字が確定された場合、エンターキーによる改行は無効化させる
      return state->input == "" ? IRV_TO_PROCESS : IRV_DO_NOTHING;
    },

    [&](const senn::fcitx::states::Converting *state) {
      senn::fcitx::ui::Converting(instance, state);
      return IRV_TO_PROCESS;
    },

    [&](const senn::fcitx::states::Editing *state) {
      senn::fcitx::ui::Editing(instance, state);
      if (sym == FcitxKey_BackSpace) {
        // IMEが文字を削除した
        //     -> OSが文字が削除するのを抑制 (IRV_DO_NOTHING)
        // IMEが文字を削除していない
        //     -> OSに文字を削除してもらう (IRV_TO_PROCESS)
        return state->consumed ? IRV_DO_NOTHING : IRV_TO_PROCESS;
      }

      // MEMO:
      //   バッファが空の状態での、上下左右の矢印キー対応
      //   とりあえずこれで動くか様子見
      return state->consumed ? IRV_TO_PROCESS : IRV_FLAG_FORWARD_KEY;
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
  senn->client = nullptr;

  FcitxIMIFace iface;
  memset(&iface, 0, sizeof(FcitxIMIFace));
  iface.Init = FcitxSennInit;
  iface.ResetIM = FcitxSennReset;
  iface.DoInput = FcitxSennDoInput;
  iface.DoReleaseInput = FcitxSennDoReleaseInput;
  iface.ReloadConfig = FcitxSennReloadConfig;

  senn::fcitx::InvokeIPCServer(SOCKET_NAME);

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
