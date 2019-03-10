#include "stdafx.h"

#include <msctf.h>
#include <combaseapi.h>

#include "text_service_registerable.h"


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

} // namespace


namespace senn {
namespace win {
namespace text_service {


// https://docs.microsoft.com/en-us/windows/desktop/tsf/text-service-registration
BOOL TextServiceRegisterable::Register(
    const TextServiceRegisterable* const obj,
    const GUID& clsid) {
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
           obj->GetProfileGuid(),
           obj->GetProfileDescription(),
           -1)) {
    return FALSE;
  }


  if (!RegisterCategories(
           clsid,
           obj->GetCategories())) {
    return FALSE;
  }

  return TRUE;
}

} // text_service
} // win
} // senn
