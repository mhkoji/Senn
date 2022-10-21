// dllmain.cpp : DLL アプリケーションのエントリ ポイントを定義します。
#include "pch.h"
#include "variable.h"
#include <ShlObj_core.h>
#include <ecl/ecl.h>
#include <sstream>

extern "C" {
void init_senn(cl_object);
}

char kKkcEnginePath[256];

void GetHomeDir(std::wstring *out) {
  PWSTR homedir_path = nullptr;
  if (SHGetKnownFolderPath(FOLDERID_Profile, KF_FLAG_DEFAULT_PATH, NULL,
                           &homedir_path) == S_OK) {
    *out = homedir_path;
    *out += L"\\";
  }
  CoTaskMemFree(homedir_path);
}

void Init() {
  std::wstring home_dir = L"";
  {
    GetHomeDir(&home_dir);
    if (home_dir == L"") {
      return;
    }
  }

  const std::wstring senn_dir = home_dir + L".senn\\";
  const std::wstring ecl_dir = senn_dir + L"ecl\\";

  _wputenv_s(L"ECLDIR", ecl_dir.c_str());

  {
    char ecl_str[16];
    strncpy_s(ecl_str, "ecl", sizeof(ecl_str));
    char *ecl[1] = {ecl_str};
    cl_boot(1, ecl);
  }

  ecl_init_module(NULL, init_senn);

  const std::wstring kkc_engine_path = senn_dir + L"kkc-engine.exe";
  size_t size;
  wcstombs_s(&size, kKkcEnginePath, sizeof(kKkcEnginePath),
             kkc_engine_path.c_str(), sizeof(kKkcEnginePath) - 1);
}

void Destroy() { cl_shutdown(); }

BOOL APIENTRY DllMain(HMODULE hModule, DWORD ul_reason_for_call,
                      LPVOID lpReserved) {
  switch (ul_reason_for_call) {
  case DLL_PROCESS_ATTACH:
    Init();
    break;
  case DLL_THREAD_ATTACH:
  case DLL_THREAD_DETACH:
    break;
  case DLL_PROCESS_DETACH:
    Destroy();
    break;
  }
  return TRUE;
}
