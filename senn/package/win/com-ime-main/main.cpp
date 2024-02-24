#include "../../../src-cpp/win/im/stateful_ime_proxy.h"
#include "../com-ime-proxy/com-ime_h.h"
#include <iostream>
#include <string>

class ComStatefulIme : public senn::win::im::StatefulIMEProxy {
public:
  // StatefulIMEProxy
  ComStatefulIme(ISennComIme *com_ime) : com_ime_(com_ime){};

  virtual void Request(const std::string &req, std::string *res) override {
    LPSTR response;
    if (FAILED(com_ime_->Request(req.c_str(), &response))) {
      return;
    }
    *res = response;
    CoTaskMemFree(response);

    std::cerr << *res << std::endl;
    return;
  }

private:
  ISennComIme *com_ime_;
};

int main(void) {
  setlocale(LC_ALL, "Japanese");

  HRESULT hr = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED);
  if (FAILED(hr)) {
    std::wcerr << "CoInitialize failed";
    return 0;
  }

  IClassFactory *factory;
  hr = CoGetClassObject(CLSID_Senn, CLSCTX_LOCAL_SERVER, nullptr,
                        IID_IClassFactory, (LPVOID *)&factory);
  if (SUCCEEDED(hr)) {
    ISennComIme *com_ime;

    hr = factory->CreateInstance(nullptr, IID_ISennComIme, (void **)&com_ime);
    if (SUCCEEDED(hr)) {

      {
        ComStatefulIme ime(com_ime);

        ime.ToggleInputMode();
        std::vector<uint64_t> syms = {84, 79, 85, 75, 89, 79, 85, 78, 73, 73,
                                      75, 73, 77, 65, 83, 73, 84, 65, 32};
        for (size_t i = 0; i < syms.size(); i++) {
          uint64_t sym = syms[i];
          BYTE modifiers = 0;
          std::cout << "Sym: " << sym << " -> ";
          ime.ProcessInput(
              sym, &modifiers,
              [&](const senn::win::im::views::Editing &v) {
                // std::wcout << "Editing: " << v.input << std::endl;
              },
              [&](const senn::win::im::views::Converting &v) {
                // std::wcout << "Converting:";
                // for (size_t i = 0; i < v.forms.size(); i++) {
                //     std::wcout << " " << v.forms[i];
                // }
                // std::wcout << std::endl;
              },
              [&](const senn::win::im::views ::Committed &v) {
                //   std::wcout << "Committed: " << v.input << std::endl;
              });
          Sleep(20);
        }
      }

      com_ime->Release();
    } else {
      std::cerr << "CreateInstance failed " << std::hex << (ULONG32)hr
                << std::endl;
    }

    factory->Release();
  } else {
    std::cerr << "CoGetClassObject failed " << std::hex << (ULONG32)hr
              << std::endl;
  }

  CoUninitialize();
  return 0;
}
