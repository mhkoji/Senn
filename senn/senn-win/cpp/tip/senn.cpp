#include "stdafx.h"

#include <vector>
#include <cstdlib>
#include <msctf.h>
#include <initguid.h>

#include "senn.h"


namespace senn {
namespace win {

/////////////////////////////////////////////////////////

void DllRegistration::COMServerRegisterable::GetCOMServerSettings(
    registry::com_server::Settings *output) const {
  output->description       = (const BYTE*) kDescription;
  output->description_bytes = (_countof(kDescription)) * sizeof(WCHAR);

  output->threading_model       = (const BYTE*) kThreadingModel;
  output->threading_model_bytes = (_countof(kThreadingModel)) * sizeof(WCHAR);
}


void DllRegistration::TextServiceRegisterable::GetRegistrationSettings(
    text_service::registration::Settings *output) const {
  output->profile_guid = kProfileGuid;
  output->profile_description = kProfileDescription;

  size_t category_count = static_cast<size_t>(sizeof(kCategories) / sizeof(kCategories[0]));
  for (size_t i = 0; i < category_count; i++) {
    output->categories.push_back(kCategories[i]);
  }
}


DllRegistration::DllRegistration() :
  com_server_(new DllRegistration::COMServerRegisterable()),
  text_service_(new DllRegistration::TextServiceRegisterable()) {
}

const GUID& DllRegistration::GetClsid() const {
  return kClsid;
}


HRESULT DllRegistration::Register(const DllRegistration *senn, HINSTANCE module_handle) {
  if (!senn->com_server_->Register(senn->GetClsid(), module_handle)) {
    DllRegistration::Unregister(senn);
    return E_FAIL;
  }

  if (!senn->text_service_->Register(senn->GetClsid())) {
    DllRegistration::Unregister(senn);
    return E_FAIL;
  }

  return S_OK;
}


HRESULT DllRegistration::Unregister(const DllRegistration *senn) {
  senn->text_service_->Unregister(senn->GetClsid());

  senn->com_server_->Unregister(senn->GetClsid());

  return S_OK;
}

/////////////////////////////////////////////////////////

} // win
} // senn
