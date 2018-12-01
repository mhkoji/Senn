#include "ipc.h"

using namespace hachee::ipc;

// g++ ipc.cpp ipc_main.cpp
int main() {
  Client *client = Client::ConnectTo("/tmp/hachee.sock");
  client->Send("This is ipc main");
  client->Close();
}
