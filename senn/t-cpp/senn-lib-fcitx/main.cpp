#include "fcitx/im/stateful_ime_ecl.h"
#include <iostream>

const char *kECLDIR = "/usr/lib/senn/fcitx/ecl/lib/ecl-21.2.1/";     // TODO

void PrintConverting(const senn::fcitx::im::views::Converting *view) {
  std::cout << "Converting:";
  for (size_t i = 0; i < view->forms.size(); i++) {
    std::cout << " " << view->forms[i];
  }
}

void PrintEditing(const senn::fcitx::im::views::Editing *view) {
  std::cout << "Editing: " << view->input;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    return 1;
  }

  setenv("ECLDIR", kECLDIR, 1);
  senn::fcitx::im::StatefulIMEEcl::ClBoot();
  senn::fcitx::im::StatefulIMEEcl::EclInitModule();

  senn::fcitx::im::StatefulIME *ime =
      senn::fcitx::im::StatefulIMEEcl::Create(std::string(argv[1]));

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

  delete ime;

  senn::fcitx::im::StatefulIMEEcl::ClShutdown();

  return 0;
}
