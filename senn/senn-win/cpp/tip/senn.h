#pragma once

#include "registry.h"
#include "text_service_registerable.h"

namespace senn {
namespace win {

class SennTextService :
  public registry::COMServerRegisterable,
  public text_service::TextServiceRegisterable {

public:
  const GUID& GetClsid() const;


private:
  const BYTE* GetDescription() const override;
  DWORD GetDescriptionBytes()   const override;

  const BYTE* GetThreadingModel() const override;
  DWORD GetThreadingModelBytes()   const override;

 
  const GUID& GetProfileGuid()  const override;
  const WCHAR* GetProfileDescription() const override;

  std::vector<GUID> GetCategories() const override;
};

extern SennTextService *g_senn_text_service;

} // win
} // senn
