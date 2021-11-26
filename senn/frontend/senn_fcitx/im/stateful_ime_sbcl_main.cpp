#include "stateful_ime_sbcl.h"
#include <iostream>

using namespace senn::fcitx::im;

void PrintConverting(const views::Converting *view) {
  std::cout << "Converting:";
  for (size_t i = 0; i < view->forms.size(); i++) {
    std::cout << " " << view->forms[i];
  }
}

void PrintEditing(const views::Editing *view) {
  std::cout << "Editing: " << view->input;
}

// ros run -s senn-bin-fcitx-lib -s sbcl-librarian -l ../../../src/bin/fcitx-lib-sbcl.lisp
// gcc -c -fpic libsennfcitx.c
// gcc --shared libsennfcitx.o -o libsennfcitx.so -lsbcl -L ~/.roswell/src/sbcl-2.1.10/src/runtime/
// g++ stateful_ime_sbcl_main.cpp stateful_ime_sbcl.cpp stateful_ime_proxy_ipc.cpp -o main -lsbcl -lsennfcitx -L ./ -L ~/.roswell/src/sbcl-2.1.10/src/runtime/ -I ../../ -I ../../../third-party/
// LD_LIBRARY_PATH=.:~/.roswell/src/sbcl-2.1.10/src/runtime/ ./main
int main(void) {
  StatefulIMESbcl::Init("libsennfcitx.core");

  StatefulIME *ime = StatefulIMESbcl::Create();

  std::vector<uint32_t> syms = {116, 111, 117, 107, 121, 111, 117,
                                110, 105, 105, 107, 105, 109, 97,
                                115, 105, 116, 97,  32};

  std::cout << std::endl;
  for (size_t i = 0; i < syms.size(); i++) {
    uint32_t sym = syms[i];
    std::cout << "Sym: " << sym << " -> ";
    ime->ProcessInput(sym, 0, 0, PrintConverting, PrintEditing);
    std::cout << std::endl;
  }
  return 0;
}
