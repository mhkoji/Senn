#include <iostream>

#include "ipc_stateful_im_proxy_server.h"

// g++ -std=c++11 ipc_stateful_im_proxy_server_main.cpp ipc_stateful_im_proxy_server.cpp
int main() {
  senn::fcitx::InvokeIPCServer("/tmp/senn.sock");
  return 0;
}
