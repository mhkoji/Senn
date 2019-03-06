#include "stdafx.h"
#include <cstdlib>
#include "variable.h"
#include "register.h"
#include "dllexports.h"

STDAPI DllRegisterServer();


STDAPI DllRegisterServer() {
  if (RegisterServer(TEXT_SERVICE_CLSID,

                    (const BYTE*) TEXT_SERVICE_DESCRIPTION,
                    (_countof(TEXT_SERVICE_DESCRIPTION)) * sizeof(WCHAR),

                    (const BYTE*) TEXT_SERVICE_THREADING_MODEL,
                    (_countof(TEXT_SERVICE_THREADING_MODEL)) * sizeof(WCHAR),
        
                    g_module_handle)) {
    return S_OK;
  }

  // DllUnregisterServer();
  return E_FAIL;
}