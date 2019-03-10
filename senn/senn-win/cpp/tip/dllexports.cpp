#include "stdafx.h"
#include <cstdlib>
#include "variable.h"
#include "register.h"
#include "dllexports.h"


STDAPI DllRegisterServer() {
  if (!RegisterCOMServer(
           TEXT_SERVICE_CLSID,

           (const BYTE*) TEXT_SERVICE_DESCRIPTION,
           (_countof(TEXT_SERVICE_DESCRIPTION)) * sizeof(WCHAR),

           (const BYTE*) TEXT_SERVICE_THREADING_MODEL,
           (_countof(TEXT_SERVICE_THREADING_MODEL)) * sizeof(WCHAR),
        
           g_module_handle)) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  if (!RegisterTextService(
           TEXT_SERVICE_CLSID,

           TEXT_SERVICE_PROFILE_GUID,
           TEXT_SERVICE_PROFILE_DESCRIPTION,
           -1,

           std::vector<GUID>(TEXT_SERVICE_CATEGORIES,
                             std::end(TEXT_SERVICE_CATEGORIES)))) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  return S_OK;
}