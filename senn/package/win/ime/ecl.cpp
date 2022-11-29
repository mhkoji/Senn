#include "./ecl.h"
#include "../../../src-cpp/win/im/stateful_ime_ecl.h"
#include <ShlObj_core.h>
#include <ecl/ecl.h>
#include <sstream>
#include <string>

#include <iostream>

extern "C" {
void init_senn(cl_object);
}

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

  char kkc_engine_path[256];
  {
    const std::wstring kkc_engine_path_string = senn_dir + L"kkc-engine.exe";
    size_t size;
    wcstombs_s(&size, kkc_engine_path, sizeof(kkc_engine_path),
               kkc_engine_path_string.c_str(), sizeof(kkc_engine_path) - 1);
  }
  senn::win::im::StatefulIMEEcl::Init(kkc_engine_path);

  std::cerr << "initialized" << std::endl;
}

void Destroy() {
  senn::win::im::StatefulIMEEcl::Destroy();
  cl_shutdown();
}

senn::win::im::StatefulIME *MakeStatefulIME() {
  return senn::win::im::StatefulIMEEcl::Create();
}
