#pragma once
#include "../../request.h"
#include "stateful_ime_proxy.h"
#include <string>
#include <windows.h>
#include <winsock2.h>

namespace senn {
namespace win {
namespace im {

class Connection {
public:
  virtual ~Connection(){};

  virtual void Close() = 0;

  virtual bool Write(const std::string &) = 0;

  virtual bool ReadLine(std::string *) = 0;
};

class ConnectionIPC : public Connection {
public:
  ConnectionIPC(HANDLE);
  // Connection
  virtual void Close() override;
  virtual bool Write(const std::string &) override;
  virtual bool ReadLine(std::string *) override;

private:
  const HANDLE pipe_;
};

class ConnectionTCP : public Connection {
public:
  ConnectionTCP(SOCKET);
  // Connection
  virtual void Close() override;
  virtual bool Write(const std::string &) override;
  virtual bool ReadLine(std::string *) override;

private:
  const SOCKET socket_;
};

class StatefulIMEConn {
  class Requester : public senn::RequesterInterface {
  public:
    Requester(Connection *conn);
    ~Requester();

  private:
    Connection *conn_;

    // RequesterInterface
    virtual void Request(const std::string &, std::string *) override;
  };

public:
  static StatefulIMEProxy *IPC(const WCHAR *const named_pipe_path);
  static StatefulIMEProxy *TCP(const std::string &host,
                               const std::string &port);
};

} // namespace im
} // namespace win
} // namespace senn
