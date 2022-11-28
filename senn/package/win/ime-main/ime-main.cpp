#include "../ime/ime_h.h"
#include <iostream>
#include <locale.h>

int main() {
  setlocale(LC_ALL, "Japanese");

  HRESULT hr = ::CoInitialize(NULL);
  if (FAILED(hr)) {
    std::cerr << "CoInitialize failed";
    return 1;
  }

  ISennIME *ime;

  hr = CoCreateInstance(CLSID_SennIME, NULL, CLSCTX_INPROC_SERVER, IID_ISennIME,
                        (void **)&ime);

  if (SUCCEEDED(hr)) {
    SennInputMode mode = SennInputMode::kUnknown;
    std::cerr << mode << std::endl;
    ime->GetInputMode(&mode);
    std::cerr << mode << std::endl;
    ime->Release();
  } else {
    std::cerr << "CCI failed " << (ULONG32)hr << std::endl;
  }

  ::CoUninitialize();
}
