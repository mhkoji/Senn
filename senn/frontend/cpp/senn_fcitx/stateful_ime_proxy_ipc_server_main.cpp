#include <iostream>

#include "stateful_ime_proxy_ipc_server.h"

// g++ -std=c++11 stateful_ime_proxy_ipc_server_main.cpp stateful_ime_proxy_ipc_server.cpp
int main() {
  senn::fcitx::StartIPCServer("/tmp/senn-server-socket");
  return 0;
}
