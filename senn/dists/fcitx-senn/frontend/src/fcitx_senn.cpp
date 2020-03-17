#include <fcitx/ime.h>
#include <sys/stat.h>
// #include <iostream>

#include "senn_fcitx/iface.h"

static void FcitxSennDestroy(void *arg) {
  senn::fcitx::iface::DestoryIM(arg);
  // std::cout << "senn-fcitx: destroyed:"
  //           << " [" << std::hex << arg << "]"
  //           << std::endl;
}

static void* FcitxSennCreate(FcitxInstance *fcitx) {
  FcitxIMIFace iface;
  memset(&iface, 0, sizeof(FcitxIMIFace));
  iface.Init = senn::fcitx::iface::Init;
  iface.ResetIM = senn::fcitx::iface::ResetIM;
  iface.DoInput = senn::fcitx::iface::DoInput;
  iface.DoReleaseInput = senn::fcitx::iface::DoReleaseInput;
  iface.ReloadConfig = senn::fcitx::iface::ReloadConfig;

  void *fcitx_im = senn::fcitx::iface::SetupIM(fcitx,
                                               "/usr/lib/senn/server");
  FcitxInstanceRegisterIMv2(
      fcitx,
      fcitx_im,
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

  return fcitx_im;
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
