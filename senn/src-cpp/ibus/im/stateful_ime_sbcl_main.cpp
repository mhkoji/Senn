#include "stateful_ime_sbcl.h"
#include <iostream>

using namespace senn::ibus::im;

void PrintConverting(const senn::fcitx::im::views::Converting *view) {
  std::cout << "Converting:";
  for (size_t i = 0; i < view->forms.size(); i++) {
    std::cout << " " << view->forms[i];
  }
}

void PrintEditing(const senn::fcitx::im::views::Editing *view) {
  std::cout << "Editing: " << view->input;
}

// clang-format off
// cp ~/.roswell/src/sbcl-2.1.10/src/runtime/libsbcl.so ./
// ros run -s senn-lib-ibus
// gcc -c -fpic libsenn_ibus.c
// gcc -shared libsenn_ibus.o -o libsenn_ibus.so -lsbcl -L. -Wl,-R ./
// g++ -I ../../ -I ../../../third-party/ stateful_ime_sbcl_main.cpp stateful_ime_sbcl.cpp stateful_ime_proxy.cpp -o main -lsenn_ibus -L. -Wl,-R ./
// ./main
// clang-format on
int main(void) {
  StatefulIMESbcl::Init("libsenn_ibus.core");

  StatefulIME *ime = StatefulIMESbcl::Create();

  std::vector<uint32_t> syms = {116, 111, 117, 107, 121, 111, 117,
                                110, 105, 105, 107, 105, 109, 97,
                                115, 105, 116, 97,  32};

  std::cout << std::endl;
  std::cout << int(ime->ToggleInputMode()) << std::endl;
  for (size_t i = 0; i < syms.size(); i++) {
    uint32_t sym = syms[i];
    std::cout << "Sym: " << sym << " -> ";
    ime->ProcessInput(sym, 0, 0, PrintConverting, PrintEditing);
    std::cout << std::endl;
  }
  return 0;
}
