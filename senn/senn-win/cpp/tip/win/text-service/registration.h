#pragma once

#include <windows.h>
#include <vector>

#include "../registry.h"


namespace senn {
namespace win {
namespace text_service {
  
namespace registration {

struct Settings {
  GUID         profile_guid;
  const WCHAR* profile_description;

  std::vector<GUID> categories;
};


BOOL Register(const GUID&, const Settings&);

void Unregister(const GUID&, const Settings&);


class SettingsProvider {
public:
  virtual ~SettingsProvider() {}

  virtual void Get(registration::Settings*) const = 0;
};

} // registration



class TextServiceRegistrar {
public:
  TextServiceRegistrar(const GUID* const);

  BOOL Register(const registration::SettingsProvider* const) const;

  void Unregister(const registration::SettingsProvider* const provider) const;

private:

  const GUID* const clsid_;
};


class DllRegistration {
public:

  DllRegistration(const GUID * const);

private:

  const registry::COMServerRegistrar* const com_server_;

  const TextServiceRegistrar* const text_service_;

public:

  static HRESULT Register(
      const DllRegistration*,
      const registry::com_server::SettingsProvider* const,
      const registration::SettingsProvider* const);

  static HRESULT Unregister(
      const DllRegistration*,
      const registration::SettingsProvider* const);
};


} // text_service
} // win
} // senn