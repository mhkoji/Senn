#include "ipc.h"

using namespace hachee::ipc;

// g++ ipc.cpp ipc_main.cpp
int main() {
  Client client("/tmp/hachee.sock");
  client.Connect();
  client.Send("This is ipc main");
  client.Close();
}
