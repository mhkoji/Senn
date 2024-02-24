#pragma once

#include <windows.h>

namespace senn {
namespace win {
namespace registry {

namespace com_server {

struct Settings {
  struct {
    const BYTE *content;
    DWORD bytes;
  } description;

  struct {
    const BYTE *content;
    DWORD bytes;
  } threading_model;

  struct {
    WCHAR content[MAX_PATH] = {'\0'};
    DWORD size_including_null_termination;
  } module_file_name;
};

BOOL Register(const GUID &, const Settings &);

void Unregister(const GUID &);

class SettingsProvider {
public:
  ~SettingsProvider() {}

  virtual BOOL Get(Settings *) const = 0;
};

} // namespace com_server

class COMServerRegistrar {
public:
  COMServerRegistrar(const GUID *const);

  BOOL Register(const com_server::SettingsProvider *const) const;

  void Unregister() const;

private:
  const GUID *const clsid_;
  ;
};

} // namespace registry
} // namespace win
} // namespace senn
