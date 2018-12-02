#include <iostream>

#include "ipc.h"

using namespace hachee::ipc;

// g++ -std=c++11 ipc.cpp ipc_main.cpp
int main() {
  Connection *conn = Connection::ConnectTo("/tmp/hachee.sock");
  conn->Write("{\"op\": \"process-key\", \"args\": {\"code\": 97}}\n");

  std::string output;
  conn->ReadLine(&output);
  std::cout << output;

  conn->Close();
}
