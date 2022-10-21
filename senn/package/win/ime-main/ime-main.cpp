#include "../../../src-cpp/win/im/stateful_ime_dll.h"
#include <iostream>
#include <locale.h>

int main() {
  setlocale(LC_ALL, "Japanese");

  senn::win::im::StatefulIME *ime = senn::win::im::StatefulIMEDll::Create();

  std::vector<uint64_t> syms = {84, 79, 85, 75, 89, 79, 85, 78, 73, 73,
                                75, 73, 77, 65, 83, 73, 84, 65, 32};
  ime->ToggleInputMode();
  for (size_t i = 0; i < syms.size(); i++) {
    uint64_t sym = syms[i];
    BYTE modifiers = 0;
    std::cout << "Sym: " << sym << " -> ";
    ime->ProcessInput(
        sym, &modifiers,
        [&](const senn::win::im::views::Editing &v) {
          std::wcout << "Editing: " << v.input;
        },
        [&](const senn::win::im::views::Converting &v) {
          std::cout << "Converting:";
          for (size_t i = 0; i < v.forms.size(); i++) {
            std::wcout << " " << v.forms[i];
          }
        },
        [&](const senn::win::im::views::Committed &v) {
          std::wcout << "Committed: " << v.input;
        });
    std::cout << std::endl;
  }

  delete ime;
}
