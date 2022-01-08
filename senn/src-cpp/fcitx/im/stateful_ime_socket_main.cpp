#include "stateful_ime_socket.h"
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
// g++ -I ../../ -I ../../../third-party/ stateful_ime_socket_main.cpp stateful_ime_socket.cpp stateful_ime_proxy.cpp ../../process/process.cpp ../../ipc/socket.cpp
// ./a.out
// clang-format on
int main(void) {
  StatefulIME *ime = StatefulIMESocket::ConnectLocalAbstractTo("/tmp/senn-server-socket");

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
