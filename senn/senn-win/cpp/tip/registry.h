#pragma once

#include "stdafx.h"
#include <windows.h>


namespace senn {
namespace win {
namespace registry {

namespace com_server {

struct Settings {
  const BYTE *description;
  DWORD       description_bytes;

  const BYTE *threading_model;
  DWORD       threading_model_bytes;
};

BOOL Register(const Settings&, const GUID&, HINSTANCE);

void Unregister(const GUID&);

} // com_server


class COMServerRegisterable {
public:
  virtual ~COMServerRegisterable() {}

  virtual void GetCOMServerSettings(com_server::Settings *) const = 0;

  BOOL Register(const GUID&, HINSTANCE) const;

  void Unregister(const GUID&) const;
};


} // registry
} // win
} // senn
