#pragma once

#include "registry.h"
#include "text_service_registration.h"

namespace senn {
namespace win {

class SennTextService :
  public registry::COMServerRegisterable,
  public text_service::TextServiceRegisterable {
public:

  const GUID& GetClsid() const;

private:

  void GetCOMServerSettings(registry::com_server::Settings*) const override;

  void GetRegistrationSettings(text_service::registration::Settings*) const override;
};

extern SennTextService *g_senn_text_service;

} // win
} // senn
