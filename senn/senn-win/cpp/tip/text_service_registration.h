#pragma once

#include <windows.h>
#include <vector>


namespace senn {
namespace win {
namespace text_service {
  
namespace registration {

struct Settings {
  GUID         profile_guid;
  const WCHAR* profile_description;

  std::vector<GUID> categories;
};


BOOL Register(const Settings&, const GUID&);

void Unregister(const Settings&, const GUID&);

} // registration


class TextServiceRegisterable {
public:
  virtual ~TextServiceRegisterable() {};

  virtual void GetRegistrationSettings(registration::Settings*) const = 0;

  BOOL Register(const GUID&) const;

  void Unregister(const GUID&) const;
};


} // text_service
} // win
} // senn