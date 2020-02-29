#include <iostream>

#include "ipc.h"

using namespace senn::ipc;

// g++ -std=c++11 ipc.cpp ipc_main.cpp
int main() {
  Connection *conn = Connection::ConnectAbstractTo(
      "/tmp/senn-server-socket");

  conn->Write(
      "{\"op\": \"input\", \"args\": {\"sym\": 97, \"state\": 0}}\n");

  std::string output;
  conn->ReadLine(&output);
  std::cout << output;

  conn->Close();
}
