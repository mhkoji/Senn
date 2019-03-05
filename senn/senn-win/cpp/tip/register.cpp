#include "stdafx.h"

#include <combaseapi.h>
#include <string>

#include "variable.h"
#include "register.h"


namespace {

BOOL CreateCLSIDKey(const GUID &clsid, std::basic_string<WCHAR> *output) {
  WCHAR clsid_buf[CLSID_STRLEN];
  if (!StringFromGUID2(clsid, clsid_buf, CLSID_STRLEN)) {
    return FALSE;
  }

  *output = L"CLSID\\";
  *output += clsid_buf;

  return TRUE;
}


class RegKeyCloser {
public:
  RegKeyCloser(HKEY key) : key_(key) {}

  ~RegKeyCloser() {
    if (key_) {
      RegCloseKey(key_);
    }
  }

private:
  const HKEY key_;
};

} // namespace

BOOL RegisterServer() {
  std::basic_string<WCHAR> clsid_key;
  if (!CreateCLSIDKey(TEXT_SERVICE_CLSID, &clsid_key)) {
    return FALSE;
  }

  HKEY clsid_handle = nullptr;
  {
    DWORD copied_strlen = 0;
    if (RegCreateKeyEx(
            HKEY_CLASSES_ROOT, clsid_key.c_str(),
            0,
            NULL,
            REG_OPTION_NON_VOLATILE,
            KEY_WRITE,
            NULL,
            &clsid_handle,
            &copied_strlen) != ERROR_SUCCESS) {
      return FALSE;
    }
  }
  RegKeyCloser clsid_closer(clsid_handle);

  if (RegSetValueEx(
         clsid_handle, NULL, 0, REG_SZ,
         (const BYTE *) TEXT_SERVICE_DESCRIPTION,
         (_countof(TEXT_SERVICE_DESCRIPTION)) * sizeof(WCHAR)) != ERROR_SUCCESS) {
    return FALSE;
  }

  HKEY in_proc_server32_handle = nullptr;
  {
    DWORD copied_strlen = 0;
    if (RegCreateKeyEx(
            clsid_handle, L"InProcServer32",
            0,
            NULL,
            REG_OPTION_NON_VOLATILE,
            KEY_WRITE,
            NULL,
            &in_proc_server32_handle,
            &copied_strlen) != ERROR_SUCCESS) {
      return FALSE;
    }
  }
  RegKeyCloser in_proc_server32_closer(in_proc_server32_handle);

  WCHAR filename[MAX_PATH] = {'\0'};
  DWORD num_chars = GetModuleFileName(g_module_handle, filename, ARRAYSIZE(filename));
  if (num_chars == 0) {
    return FALSE;
  }
  DWORD num_chars_including_null_termination = num_chars < (MAX_PATH - 1) ? num_chars + 1 : MAX_PATH;

  if (RegSetValueEx(
          in_proc_server32_handle, NULL, 0, REG_SZ,
          (const BYTE *)filename,
          (num_chars_including_null_termination) * sizeof(WCHAR)) != ERROR_SUCCESS) {
    return FALSE;
  }

  if (RegSetValueEx(
          in_proc_server32_handle, L"ThreadingModel", 0, REG_SZ,
          (const BYTE *)TEXT_SERVICE_THREADING_MODEL,
          (_countof(TEXT_SERVICE_THREADING_MODEL)) * sizeof(WCHAR)) != ERROR_SUCCESS) {
    return FALSE;
  }

  return TRUE;
}
