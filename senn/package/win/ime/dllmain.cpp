// dllmain.cpp : DLL アプリケーションのエントリ ポイントを定義します。
#include "./ecl.h"
#include "ime_impl.h"
#include "pch.h"
#include <OleCtl.h>

LONG g_locks = 0;
static HINSTANCE g_module_handle;

static const char *g_entries[][3] = {
    {"CLSID\\{b4574cd7-53ef-4ec6-873d-6d30a5ccccad}", 0, "Senn IME"},
    {"CLSID\\{b4574cd7-53ef-4ec6-873d-6d30a5ccccad}\\InprocServer32", 0,
     (const char *)-1},
    {"CLSID\\{b4574cd7-53ef-4ec6-873d-6d30a5ccccad}\\InprocServer32",
     "ThreadingModel", "Apartment"},

};

STDAPI DllUnregisterServer(void) {
  HRESULT hr = S_OK;
  int nEntries = sizeof(g_entries) / sizeof(*g_entries);
  for (int i = nEntries - 1; i >= 0; i--) {
    long err = RegDeleteKeyA(HKEY_CLASSES_ROOT, g_entries[i][0]);
    if (err != ERROR_SUCCESS) {
      hr = S_FALSE;
    }
  }

  return hr;
}

STDAPI DllRegisterServer(void) {
  char szFileName[MAX_PATH];
  GetModuleFileNameA(g_module_handle, szFileName, MAX_PATH);

  int entry_count = sizeof(g_entries) / sizeof(*g_entries);
  for (int i = 0; i < entry_count; i++) {
    const char *pszKeyName = g_entries[i][0];
    const char *pszValueName = g_entries[i][1];
    const char *pszValue = g_entries[i][2];

    if (pszValue == (const char *)-1) {
      pszValue = szFileName;
    }

    HKEY hkey;
    long err = RegCreateKeyA(HKEY_CLASSES_ROOT, pszKeyName, &hkey);
    if (ERROR_SUCCESS == err) {
      err =
          RegSetValueExA(hkey, pszValueName, 0, REG_SZ, (const BYTE *)pszValue,
                         static_cast<DWORD>((strlen(pszValue) + 1)));
      RegCloseKey(hkey);
    } else if (ERROR_SUCCESS != err) {
      DllUnregisterServer();
      return SELFREG_E_CLASS;
    }
  }
  return S_OK;
}

BOOL APIENTRY DllMain(HMODULE hModule, DWORD ul_reason_for_call,
                      LPVOID lpReserved) {
  switch (ul_reason_for_call) {
  case DLL_PROCESS_ATTACH:
    g_module_handle = hModule;
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

STDAPI DllCanUnloadNow(void) { return 0 == g_locks ? S_OK : S_FALSE; }

STDAPI DllGetClassObject(_In_ REFCLSID rclsid, _In_ REFIID riid,
                         _Outptr_ LPVOID FAR *ppv) {
  if (rclsid != CLSID_SennIME) {
    return CLASS_E_CLASSNOTAVAILABLE;
  }
  IMEClassFactory *factory = new IMEClassFactory(MakeStatefulIME);
  if (!factory) {
    return E_OUTOFMEMORY;
  }
  HRESULT hr = factory->QueryInterface(riid, ppv);
  factory->Release();
  return hr;
}
