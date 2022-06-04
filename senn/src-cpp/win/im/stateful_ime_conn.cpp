#include "stateful_ime_conn.h"
#include <iostream>
// #include <ws2tcpip.h>

namespace senn {
namespace win {
namespace im {

ConnectionIPC::ConnectionIPC(const HANDLE pipe) : pipe_(pipe) {}

void ConnectionIPC::Close() { CloseHandle(pipe_); }

bool ConnectionIPC::Write(const std::string &content) {
  DWORD bytes_written;
  return WriteFile(pipe_, content.c_str(), static_cast<DWORD>(content.size()),
                   &bytes_written, NULL);
}

bool ConnectionIPC::ReadLine(std::string *output) {
  char buf[1024] = {'\0'};

  while (1) {
    DWORD bytes_read;
    if (!ReadFile(pipe_, buf, sizeof(buf), &bytes_read, NULL)) {
      return false;
    }

    *output += std::string(buf, bytes_read);

    if (buf[bytes_read - 1] == '\n') {
      break;
    }
  }

  return true;
}

/*
ConnectionTCP::ConnectionTCP(SOCKET socket) : socket_(socket) {}

void ConnectionTCP::Close() { closesocket(socket_); }

bool ConnectionTCP::Write(const std::string &content) {
  const std::string data = content + "\n"; // Needs newline
  return send(socket_, data.c_str(), static_cast<int>(data.size()), 0) !=
         SOCKET_ERROR;
}

bool ConnectionTCP::ReadLine(std::string *output) {
  char buf[1024] = {'\0'};

  while (1) {
    int bytes_read = recv(socket_, buf, sizeof(buf), 0);
    if (bytes_read < 0) {
      return false;
    }

    *output += std::string(buf, bytes_read);

    if (buf[bytes_read - 1] == '\n') {
      break;
    }
  }

  return true;
}
*/

StatefulIMEConn::Requester::Requester(Connection *conn) : conn_(conn) {}

StatefulIMEConn::Requester::~Requester() { conn_->Close(); }

void StatefulIMEConn::Requester::Request(const std::string &req,
                                         std::string *res) {
  if (!conn_->Write(req)) {
    std::cerr << "Failed to send request" << std::endl;
    std::exit(1);
  }

  if (!conn_->ReadLine(res)) {
    std::cerr << "Failed to receive response" << std::endl;
    std::exit(1);
  }
}

StatefulIMEProxy *StatefulIMEConn::IPC(const WCHAR *const named_pipe_path) {
  HANDLE pipe = CreateFile(named_pipe_path, GENERIC_READ | GENERIC_WRITE, 0,
                           NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (pipe == INVALID_HANDLE_VALUE) {
    return nullptr;
  }
  return new StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>(
      new StatefulIMEConn::Requester(new ConnectionIPC(pipe))));
}

/*
StatefulIMEProxy *StatefulIMEConn::TCP(const std::string &host,
                                       const std::string &port) {
  // https://docs.microsoft.com/ja-jp/windows/win32/winsock/complete-client-code
  struct addrinfo hint, *result = nullptr;
  ZeroMemory(&hint, sizeof(hint));
  hint.ai_family = AF_UNSPEC;
  hint.ai_socktype = SOCK_STREAM;
  hint.ai_protocol = IPPROTO_TCP;
  if (getaddrinfo(host.c_str(), port.c_str(), &hint, &result) != 0) {
    WSACleanup();
    return nullptr;
  }

  for (struct addrinfo *p = result; p != nullptr; p = p->ai_next) {
    SOCKET sock = socket(p->ai_family, p->ai_socktype, p->ai_protocol);
    if (sock == INVALID_SOCKET) {
      break;
    }
    if (connect(sock, p->ai_addr, static_cast<int>(p->ai_addrlen)) !=
        SOCKET_ERROR) {
      freeaddrinfo(result);
      return new StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>(
          new StatefulIMEConn::Requester(new ConnectionTCP(sock))));
    }
    closesocket(sock);
  }
  freeaddrinfo(result);
  WSACleanup();
  return nullptr;
}
*/

} // namespace im
} // namespace win
} // namespace senn
