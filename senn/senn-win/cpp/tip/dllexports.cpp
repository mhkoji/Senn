#include "stdafx.h"
#include <cstdlib>
#include "registry.h"
#include "text_service_registration.h"
#include "settings.h"
#include "variable.h"


STDAPI DllRegisterServer() {
  if (!senn::win::registry::RegisterCOMServer(
           senn::win::settings::kClsid,

           (const BYTE*) senn::win::settings::kDescription,
           (_countof(senn::win::settings::kDescription)) * sizeof(WCHAR),

           (const BYTE*)senn::win::settings::kThreadingModel,
           (_countof(senn::win::settings::kThreadingModel)) * sizeof(WCHAR),
        
           g_module_handle)) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  if (!senn::win::text_service_registration::Register(
           senn::win::settings::kClsid,

           senn::win::settings::kProfileGuid,
           senn::win::settings::kProfileDescription,
           -1,

           std::vector<GUID>(senn::win::settings::kCategories,
                             std::end(senn::win::settings::kCategories)))) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  return S_OK;
}