#include "stdafx.h"

#include <vector>
#include <cstdlib>
#include <msctf.h>
#include <initguid.h>

#include "senn.h"


namespace senn {
namespace win {

const GUID& SennRegistration::GetClsid() const {
  return kClsid;
}


void SennRegistration::GetCOMServerSettings(registry::com_server::Settings *output) const {
  output->description       = (const BYTE*) kDescription;
  output->description_bytes = (_countof(kDescription)) * sizeof(WCHAR);

  output->threading_model       = (const BYTE*) kThreadingModel;
  output->threading_model_bytes = (_countof(kThreadingModel)) * sizeof(WCHAR);
}


void SennRegistration::GetRegistrationSettings(text_service::registration::Settings *output) const {
  output->profile_guid = kProfileGuid;
  output->profile_description = kProfileDescription;

  size_t category_count = static_cast<size_t>(sizeof(kCategories) / sizeof(kCategories[0]));
  for (size_t i = 0; i < category_count; i++) {
    output->categories.push_back(kCategories[i]);
  }
}


HRESULT SennRegistration::Register(const SennRegistration *senn, HINSTANCE module_handle) {
  if (!senn->RegisterCOMServer(senn->GetClsid(), module_handle)) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  if (!senn->RegisterTextService(senn->GetClsid())) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  return S_OK;
}


} // win
} // senn
