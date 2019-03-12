#include "stdafx.h"

#include <windows.h>
#include <combaseapi.h>

#include <vector>

#include "registry.h"


namespace senn {
namespace win {
namespace registry {

namespace {

// strlen("{xxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxx}")
static const size_t CLSID_STRLEN = 38;


BOOL CreateCLSIDKey(const GUID &clsid, std::basic_string<WCHAR> *output) {
  // +1 for '\0' at the end of the string
  WCHAR clsid_buf[CLSID_STRLEN + 1];
  if (!StringFromGUID2(clsid, clsid_buf, (_countof(clsid_buf)))) {
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


LONG RecurseDeleteKey(HKEY hParentKey, LPCTSTR lpszKey) {
  HKEY hKey;
  if (RegOpenKey(hParentKey, lpszKey, &hKey) != ERROR_SUCCESS) {
    return ERROR_SUCCESS; // let's assume we couldn't open it because it's not there
  }
  RegKeyCloser clsid_closer(hKey);

  LONG lRes = ERROR_SUCCESS;
  TCHAR szBuffer[256];
  DWORD dwSize = ARRAYSIZE(szBuffer);
  FILETIME time;
  while (RegEnumKeyEx(hKey, 0, szBuffer, &dwSize, NULL, NULL, NULL, &time) == ERROR_SUCCESS) {
    szBuffer[ARRAYSIZE(szBuffer) - 1] = '\0';
    lRes = RecurseDeleteKey(hKey, szBuffer);
    if (lRes != ERROR_SUCCESS) {
      return lRes;
    }
    dwSize = ARRAYSIZE(szBuffer);
  }
  return RegDeleteKey(hParentKey, lpszKey);
}

}  // namespace


namespace com_server {

BOOL Register(
    const Settings &settings,
    const GUID &clsid,
    const HINSTANCE module_handle) {
  // Create clsid key CLSID\{xxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxx}"
  std::basic_string<WCHAR> clsid_key_string;
  if (!CreateCLSIDKey(clsid, &clsid_key_string)) {
    return FALSE;
  }

  HKEY hkey_clsid = nullptr;
  if (RegCreateKeyEx(
          HKEY_CLASSES_ROOT, clsid_key_string.c_str(),
          0,
          NULL,
          REG_OPTION_NON_VOLATILE,
          KEY_WRITE,
          NULL,
          &hkey_clsid,
          NULL) != ERROR_SUCCESS) {
    return FALSE;
  }
  RegKeyCloser clsid_closer(hkey_clsid);

  // Set description as default value of the clsid key
  if (RegSetValueEx(hkey_clsid, NULL, 0, REG_SZ, 
                    settings.description,
                    settings.description_bytes) != ERROR_SUCCESS) {
    return FALSE;
  }

  // Create InProcServer32 key in the clsid key
  HKEY hkey_in_proc_server32 = nullptr;
  if (RegCreateKeyEx(
          hkey_clsid, L"InProcServer32",
          0,
          NULL,
          REG_OPTION_NON_VOLATILE,
          KEY_WRITE,
          NULL,
          &hkey_in_proc_server32,
          NULL) != ERROR_SUCCESS) {
    return FALSE;
  }
  RegKeyCloser in_proc_server32_closer(hkey_in_proc_server32);

  // Set module path as default value of the InProcServer32 key 
  {
    WCHAR filename[MAX_PATH] = { '\0' };
    DWORD num_chars = GetModuleFileName(module_handle, filename, ARRAYSIZE(filename));
    if (num_chars == 0) {
      return FALSE;
    }
    DWORD num_chars_including_null_termination =
      num_chars < (MAX_PATH - 1) ? num_chars + 1 : MAX_PATH;

    if (RegSetValueEx(
            hkey_in_proc_server32, NULL, 0, REG_SZ,
            (const BYTE *)filename,
            (num_chars_including_null_termination) * sizeof(WCHAR)) != ERROR_SUCCESS) {
      return FALSE;
    }
  }

  // Add threading model to the InProcServer32 key
  if (RegSetValueEx(hkey_in_proc_server32, L"ThreadingModel", 0, REG_SZ,
                    settings.threading_model,
                    settings.threading_model_bytes) != ERROR_SUCCESS) {
    return FALSE;
  }

  return TRUE;
}



void Unregister(const GUID &clsid) {
  std::basic_string<WCHAR> clsid_key_string;
  if (!CreateCLSIDKey(clsid, &clsid_key_string)) {
    return;
  }

  RecurseDeleteKey(HKEY_CLASSES_ROOT, clsid_key_string.c_str());
}


} // com_server


BOOL COMServerRegisterable::Register(const GUID &clsid, HINSTANCE module_handle) const {
  com_server::Settings settings;
  GetCOMServerSettings(&settings);
  return com_server::Register(settings, clsid, module_handle);
}
 
void COMServerRegisterable::Unregister(const GUID &clsid) const {
  com_server::Unregister(clsid);
}


} // registry
} // win
} // senn
