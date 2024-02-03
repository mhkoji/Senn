#include "ibus/im/stateful_ime_ecl.h"
#include <iostream>

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

  senn::ibus::im::StatefulIMEEcl::ClBoot();
  senn::ibus::im::StatefulIMEEcl::EclInitModule();

  senn::ibus::im::StatefulIME *ime =
      senn::ibus::im::StatefulIMEEcl::Create(std::string(argv[1]));

  assert(ime->ToggleInputMode() == senn::ibus::im::InputMode::kHiragana);
  assert(ime->ToggleInputMode() == senn::ibus::im::InputMode::kDirect);
  assert(ime->ToggleInputMode() == senn::ibus::im::InputMode::kHiragana);

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

  senn::ibus::im::StatefulIMEEcl::ClShutdown();

  return 0;
}
