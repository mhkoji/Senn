#include "ipc.h"

using namespace hachee::ipc;

// g++ ipc.cpp ipc_main.cpp
int main() {
  Connection *conn = Connection::ConnectTo("/tmp/hachee.sock");
  conn->Send("This is ipc main");
  conn->Close();
}
