#include "stdafx.h"

#include <msctf.h>
#include <combaseapi.h>

#include "text_service_registration.h"

namespace senn {
namespace win {
namespace text_service {

namespace {

class ObjectReleaser {
public:
  ObjectReleaser(IUnknown *pobj) : pobj(pobj) {}

  ~ObjectReleaser() {
    if (pobj) {
      pobj->Release();
    }
  }

private:
  IUnknown *pobj;
};


BOOL RegisterLanguageProfile(ITfInputProcessorProfiles *profiles,
                             const GUID &clsid,
                             const GUID &profile_guid,
                             const WCHAR *profile_description,
                             const ULONG profile_description_len) {
  HRESULT result = profiles->AddLanguageProfile(
    clsid,
    MAKELANGID(LANG_JAPANESE, SUBLANG_DEFAULT),
    profile_guid,
    profile_description,
    profile_description_len,
    NULL, 0, 0
  );

  return result == S_OK;
}


BOOL RegisterCategories(const GUID &clsid,
                        const std::vector<GUID> &categories) {
  ITfCategoryMgr *category_mgr;
  {
    HRESULT result = CoCreateInstance(
        CLSID_TF_CategoryMgr,
        NULL,
        CLSCTX_INPROC_SERVER,
        IID_ITfCategoryMgr,
        (void**)&category_mgr
    );

    if (result != S_OK) {
      return FALSE;
    }
  }
  ObjectReleaser releaser(category_mgr);

  for (std::vector<GUID>::const_iterator it = categories.begin();
       it != categories.end(); ++it) {
    if (category_mgr->RegisterCategory(clsid, *it, clsid) != S_OK) {
      return FALSE;
    }
  }

  return TRUE;
}

BOOL UnregisterCategories(const GUID &clsid,
                          const std::vector<GUID> &categories) {
  ITfCategoryMgr *category_mgr;
  {
    HRESULT result = CoCreateInstance(
      CLSID_TF_CategoryMgr,
      NULL,
      CLSCTX_INPROC_SERVER,
      IID_ITfCategoryMgr,
      (void**)&category_mgr
    );

    if (result != S_OK) {
      return FALSE;
    }
  }
  ObjectReleaser releaser(category_mgr);

  for (std::vector<GUID>::const_iterator it = categories.begin();
    it != categories.end(); ++it) {
    if (category_mgr->UnregisterCategory(clsid, *it, clsid) != S_OK) {
      return FALSE;
    }
  }

  return TRUE;
}


} // namespace


namespace registration {

// https://docs.microsoft.com/en-us/windows/desktop/tsf/text-service-registration
BOOL Register(const GUID& clsid, const Settings &settings) {
  ITfInputProcessorProfiles *profiles;
  {
    HRESULT result = CoCreateInstance(CLSID_TF_InputProcessorProfiles,
                                      NULL,
                                      CLSCTX_INPROC_SERVER,
                                      IID_ITfInputProcessorProfiles,
                                      (void**)&profiles);
    if (result != S_OK) {
      return FALSE;
    }
  }
  ObjectReleaser releaser(profiles);

  if (profiles->Register(clsid) != S_OK) {
    return FALSE;
  }

  if (!RegisterLanguageProfile(
           profiles,
           clsid,
           settings.profile_guid,
           settings.profile_description,
           -1)) {
    return FALSE;
  }


  if (!RegisterCategories(
           clsid,
           settings.categories)) {
    return FALSE;
  }

  return TRUE;
}

void Unregister(const GUID& clsid, const Settings &settings) {
  UnregisterCategories(clsid, settings.categories);

  ITfInputProcessorProfiles *profiles;
  {
    HRESULT result = CoCreateInstance(CLSID_TF_InputProcessorProfiles,
                                      NULL,
                                      CLSCTX_INPROC_SERVER,
                                      IID_ITfInputProcessorProfiles,
                                      (void**)&profiles);
    if (result != S_OK) {
      return;
    }
  }
  ObjectReleaser releaser(profiles);

  profiles->Unregister(clsid);
 }


} // registration


TextServiceRegistrar::TextServiceRegistrar(const GUID* const clsid)
  : clsid_(clsid) {
}

BOOL TextServiceRegistrar::Register(
    const registration::SettingsProvider* const provider) const {
  registration::Settings settings;
  provider->Get(&settings);
  return registration::Register(*clsid_, settings);
}


void TextServiceRegistrar::Unregister(
    const registration::SettingsProvider* const provider) const {
  registration::Settings settings;
  provider->Get(&settings);
  registration::Unregister(*clsid_, settings);
}


/////////////////////////////////////////////////////////


DllRegistration::DllRegistration(const GUID* const clsid)
  : com_server_(new registry::COMServerRegistrar(clsid)),
    text_service_(new TextServiceRegistrar(clsid)) {
}


HRESULT DllRegistration::Register(
    const DllRegistration *reg,
    const registry::com_server::SettingsProvider* const com_server_settings_provider,
    const text_service::registration::SettingsProvider* const text_service_settings_provider) {
  if (!reg->com_server_->Register(com_server_settings_provider)) {
    DllRegistration::Unregister(reg, text_service_settings_provider);
    return E_FAIL;
  }

  if (!reg->text_service_->Register(text_service_settings_provider)) {
    DllRegistration::Unregister(reg, text_service_settings_provider);
    return E_FAIL;
  }

  return S_OK;
}


HRESULT DllRegistration::Unregister(
    const DllRegistration *reg,
    const text_service::registration::SettingsProvider* const provider) {
  reg->text_service_->Unregister(provider);

  reg->com_server_->Unregister();

  return S_OK;
}


} // text_service
} // win
} // senn
