#include "stateful_ime_ecl.h"
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

// clang-format off
// ros run -e '(asdf:make-build :senn-bin-fcitx-lib :type :static-library :move-here #P"./" :monolithic t :init-name "init_senn")' -q
// g++ `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --cflags` -I../../../third-party -I../../ stateful_ime_ecl.cpp stateful_ime_proxy.cpp stateful_ime_ecl_main.cpp senn-lib-fcitx--all-systems.a `~/.roswell/impls/x86-64/linux/ecl/21.2.1/bin/ecl-config --ldflags` -lecl -o main
// ./main
// clang-format on
int main(void) {
  StatefulIMEEcl::ClBoot();
  StatefulIMEEcl::EclInitModule();

  StatefulIME *ime = StatefulIMEEcl::Create();

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

  StatefulIMEEcl::ClShutdown();
  return 0;
}
