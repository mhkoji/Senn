#include "../../../../third-party/picojson/picojson.h"
#include "../com-ime-proxy/com-ime_h.h"
#include "pch.h"
#include <OleCtl.h>
#include <ShlObj_core.h>
#include <string>
#include <strsafe.h>

static LONG g_locks = 0;
static HINSTANCE g_module_handle = NULL;
static HANDLE g_ime_server_stdin_read = NULL;
static HANDLE g_ime_server_stdin_write = NULL;
static HANDLE g_ime_server_stdout_read = NULL;
static HANDLE g_ime_server_stdout_write = NULL;
// static bool g_console_initialized = false;

namespace {
/*
void ShowConsole() {
  if (!g_console_initialized) {
    FILE *fp;
    AllocConsole();
    freopen_s(&fp, "CONOUT$", "w", stdout);
    freopen_s(&fp, "CONOUT$", "w", stderr);
    g_console_initialized = true;
  }
}
*/

void GetHomeDir(std::wstring *out) {
  PWSTR homedir_path = nullptr;
  if (SHGetKnownFolderPath(FOLDERID_Profile, KF_FLAG_DEFAULT_PATH, NULL,
                           &homedir_path) == S_OK) {
    *out = homedir_path;
    *out += L"\\";
  }
  CoTaskMemFree(homedir_path);
}

} // namespace

namespace ime_server {

void CloseServerProcessHandles() {
  HANDLE *handles[] = {&g_ime_server_stdin_read, &g_ime_server_stdin_write,
                       &g_ime_server_stdout_read, &g_ime_server_stdout_write};
  for (size_t i = 0; ARRAYSIZE(handles); i++) {
    if (*handles[i] != NULL) {
      CloseHandle(*handles[i]);
      *handles[i] = NULL;
    }
  }
}

bool EnsureServerProcessCreated() {
  if (g_ime_server_stdin_read != NULL && g_ime_server_stdin_write != NULL &&
      g_ime_server_stdout_read != NULL && g_ime_server_stdout_write != NULL) {
    return true;
  }

  // https://learn.microsoft.com/en-us/windows/win32/procthread/creating-a-child-process-with-redirected-input-and-output?source=recommendations
  SECURITY_ATTRIBUTES se_attr;
  se_attr.nLength = sizeof(SECURITY_ATTRIBUTES);
  se_attr.bInheritHandle = TRUE;
  se_attr.lpSecurityDescriptor = NULL;

  // Create a pipe for the child process's STDOUT.
  if (!CreatePipe(&g_ime_server_stdout_read, &g_ime_server_stdout_write,
                  &se_attr, 0)) {
    return false;
  }
  // Ensure the read handle to the pipe for STDOUT is not inherited.
  if (!SetHandleInformation(g_ime_server_stdout_read, HANDLE_FLAG_INHERIT, 0)) {
    return false;
  }
  // Create a pipe for the child process's STDIN.
  if (!CreatePipe(&g_ime_server_stdin_read, &g_ime_server_stdin_write, &se_attr,
                  0)) {
    return false;
  }
  // Ensure the write handle to the pipe for STDIN is not inherited.
  if (!SetHandleInformation(g_ime_server_stdin_write, HANDLE_FLAG_INHERIT, 0)) {
    return false;
  }

  PROCESS_INFORMATION proc_info;
  STARTUPINFO startup_info;

  // Set up members of the PROCESS_INFORMATION structure.
  ZeroMemory(&proc_info, sizeof(PROCESS_INFORMATION));

  // Set up members of the STARTUPINFO structure.
  // This structure specifies the STDIN and STDOUT handles for redirection.
  ZeroMemory(&startup_info, sizeof(STARTUPINFO));
  startup_info.cb = sizeof(STARTUPINFO);
  startup_info.hStdError = g_ime_server_stdout_write;
  startup_info.hStdOutput = g_ime_server_stdout_write;
  startup_info.hStdInput = g_ime_server_stdin_read;
  startup_info.dwFlags |= STARTF_USESTDHANDLES | STARTF_USESHOWWINDOW;
  startup_info.wShowWindow = SW_HIDE;

  std::wstring server_path;
  {
    std::wstring home_dir;
    GetHomeDir(&home_dir);
    server_path = home_dir + +L".senn\\ime-server.exe";
  }

  // Create the child process.
  if (!CreateProcess(server_path.c_str(),
                     NULL,          // command line
                     NULL,          // process security attributes
                     NULL,          // primary thread security attributes
                     TRUE,          // handles are inherited
                     0,             // creation flags
                     NULL,          // use parent's environment
                     NULL,          // use parent's current directory
                     &startup_info, // STARTUPINFO pointer
                     &proc_info     // receives PROCESS_INFORMATION
                     )) {
    CloseHandle(proc_info.hProcess);
    CloseHandle(proc_info.hThread);
    CloseServerProcessHandles();
    return false;
  }

  return true;
}

bool SendString(const std::string &in, std::string *out) {
  {
    DWORD bytes_written;
    if (!WriteFile(g_ime_server_stdin_write, in.c_str(),
                   static_cast<DWORD>(in.size()), &bytes_written, NULL)) {
      return false;
    }
  }

  {
    char buf[1024] = {'\0'};

    while (1) {
      DWORD bytes_read;

      if (!ReadFile(g_ime_server_stdout_read, buf, sizeof(buf), &bytes_read,
                    NULL)) {
        return false;
      }

      *out += std::string(buf, bytes_read);

      if (buf[bytes_read - 1] == '\n') {
        break;
      }
    }

    return true;
  }
}

bool MakeIme(std::string *id) {
  std::string resp;
  picojson::object obj;
  obj["method"] = picojson::value("make-ime");
  if (!SendString(picojson::value(obj).serialize() + "\n", &resp)) {
    return false;
  }

  picojson::value v;
  picojson::parse(v, resp);
  *id = v.get<picojson::object>()["id"].get<std::string>();
  return true;
}

bool HandleRequest(const std::string &id, const std::string &req,
                   std::string *res) {
  picojson::object obj;
  obj["method"] = picojson::value("handle-request");
  obj["params"] = picojson::value(picojson::object());
  obj["params"].get<picojson::object>()["id"] = picojson::value(id);
  obj["params"].get<picojson::object>()["request"] = picojson::value(req);
  return SendString(picojson::value(obj).serialize() + "\n", res);
}

void CloseIme(const std::string &id) {
  picojson::object obj;
  obj["method"] = picojson::value("close-ime");
  obj["params"] = picojson::value(picojson::object());
  obj["params"].get<picojson::object>()["id"] = picojson::value(id);
  std::string temp;
  SendString(picojson::value(obj).serialize() + "\n", &temp);
}

} // namespace ime_server

class ComIme : public ISennComIme {
public:
  ComIme(const std::string &ime_id) : count_(1), ime_id_(ime_id) {}

  ~ComIme() { ime_server::CloseIme(ime_id_); }

  // ISennComIme
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }

    if (riid == IID_IUnknown || riid == IID_ISennComIme) {
      *ppvObject = static_cast<ISennComIme *>(this);
    } else {
      return E_NOINTERFACE;
    }
    AddRef();

    return S_OK;
  }
  virtual ULONG __stdcall AddRef(void) override {
    return InterlockedIncrement(&count_);
  }
  virtual ULONG __stdcall Release(void) override {
    ULONG count = InterlockedDecrement(&count_);
    if (count == 0) {
      delete this;
    }
    return count;
  }
  virtual HRESULT __stdcall Request(LPCSTR req, LPSTR *res) override {
    if (res == nullptr) {
      return E_INVALIDARG;
    }
    std::string response;
    if (!ime_server::HandleRequest(ime_id_, std::string(req), &response)) {
      return E_FAIL;
    }
    size_t size = sizeof(CHAR) * (response.size() + 1);
    *res = static_cast<LPSTR>(CoTaskMemAlloc(size));
    if (*res == nullptr) {
      return E_OUTOFMEMORY;
    }
    return StringCbCopyA(*res, size, response.c_str());
  }

private:
  ULONG count_;

  const std::string ime_id_;
};

class ComImeClassFactory : public IClassFactory {
public:
  ComImeClassFactory() : count_(1) {}

  // IClassFactory
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }

    if (riid == IID_IUnknown || riid == IID_IClassFactory) {
      *ppvObject = static_cast<IClassFactory *>(this);
    } else {
      return E_NOINTERFACE;
    }
    AddRef();

    return S_OK;
  }
  virtual ULONG __stdcall AddRef(void) override {
    LockModule();
    return InterlockedIncrement(&count_);
  }
  virtual ULONG __stdcall Release(void) override {
    ULONG count = InterlockedDecrement(&count_);
    if (count == 0) {
      UnlockModule();
      delete this;
    }
    return count;
  }
  virtual HRESULT __stdcall CreateInstance(IUnknown *pUnkOuter, REFIID riid,
                                           void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }
    if (pUnkOuter != nullptr) {
      return CLASS_E_NOAGGREGATION;
    }

    if (!ime_server::EnsureServerProcessCreated()) {
      return E_FAIL;
    }

    std::string ime_id;
    if (!ime_server::MakeIme(&ime_id)) {
      return E_FAIL;
    }

    *ppvObject = nullptr;

    ISennComIme *ime = new ComIme(ime_id);
    if (ime == nullptr) {
      return E_OUTOFMEMORY;
    }

    HRESULT result = ime->QueryInterface(riid, ppvObject);
    ime->Release();

    return result;
  }
  virtual HRESULT __stdcall LockServer(BOOL fLock) override {
    if (fLock) {
      LockModule();
    } else {
      UnlockModule();
    }
    return S_OK;
  }

private:
  ULONG count_;

  void LockModule() { InterlockedIncrement(&g_locks); }

  void UnlockModule() { InterlockedDecrement(&g_locks); }
};

// UUIDs are copied from com-ecl.idl
static const char *g_entries[][3] = {
    {"CLSID\\{BB56A7D4-F240-4494-A081-354F4F2BA873}", 0, "com-ime-server"},
    {"CLSID\\{BB56A7D4-F240-4494-A081-354F4F2BA873}", "AppID",
     "{8CAA920A-3D78-439A-A1A4-353B85639F6E}"},
    {"CLSID\\{BB56A7D4-F240-4494-A081-354F4F2BA873}\\InprocServer32", 0,
     "<filename>"},
    {"CLSID\\{BB56A7D4-F240-4494-A081-354F4F2BA873}\\InprocServer32",
     "ThreadingModel", "Apartment"},
    {"AppID\\{8CAA920A-3D78-439A-A1A4-353B85639F6E}", "DllSurrogate", ""},
};

STDAPI DllUnregisterServer(void) {
  HRESULT hr = S_OK;
  int nEntries = sizeof(g_entries) / sizeof(*g_entries);
  for (int i = nEntries - 1; i >= 0; i--) {
    long err = RegDeleteKeyA(HKEY_CLASSES_ROOT, g_entries[i][0]);
    if (err != ERROR_SUCCESS) {
      hr = S_FALSE;
    }
  }

  return hr;
}

STDAPI DllRegisterServer(void) {
  char modue_file_name[MAX_PATH];
  GetModuleFileNameA(g_module_handle, modue_file_name, MAX_PATH);

  int entry_count = sizeof(g_entries) / sizeof(*g_entries);
  for (int i = 0; i < entry_count; i++) {
    const char *key_name = g_entries[i][0];
    const char *value_name = g_entries[i][1];
    std::string value = g_entries[i][2];

    if (value == "<filename>") {
      value = modue_file_name;
    }

    HKEY hkey;
    long err = RegCreateKeyA(HKEY_CLASSES_ROOT, key_name, &hkey);
    if (ERROR_SUCCESS == err) {
      err = RegSetValueExA(hkey, value_name, 0, REG_SZ,
                           (const BYTE *)value.c_str(),
                           static_cast<DWORD>(value.size() + 1));
      RegCloseKey(hkey);
    } else if (ERROR_SUCCESS != err) {
      DllUnregisterServer();
      return SELFREG_E_CLASS;
    }
  }
  return S_OK;
}

STDAPI DllCanUnloadNow(void) { return 0 == g_locks ? S_OK : S_FALSE; }

STDAPI DllGetClassObject(_In_ REFCLSID rclsid, _In_ REFIID riid,
                         _Outptr_ LPVOID FAR *ppv) {
  if (rclsid != CLSID_Senn) {
    return CLASS_E_CLASSNOTAVAILABLE;
  }

  ComImeClassFactory *factory = new ComImeClassFactory();
  if (!factory) {
    return E_OUTOFMEMORY;
  }
  HRESULT hr = factory->QueryInterface(riid, ppv);
  factory->Release();
  return hr;
}

BOOL APIENTRY DllMain(HMODULE hModule, DWORD ul_reason_for_call,
                      LPVOID lpReserved) {
  switch (ul_reason_for_call) {
  case DLL_PROCESS_ATTACH:
    g_module_handle = hModule;
    break;
  case DLL_THREAD_ATTACH:
  case DLL_THREAD_DETACH:
    break;
  case DLL_PROCESS_DETACH:
    ime_server::CloseServerProcessHandles();
    break;
  }
  return TRUE;
}
