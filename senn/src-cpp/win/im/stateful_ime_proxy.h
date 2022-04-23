#pragma once

#include <windows.h>
#include <winsock2.h>

#include "stateful_ime.h"

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

class StatefulIMEProxy : public StatefulIME {
public:
  ~StatefulIMEProxy() override;
  virtual bool CanProcess(uint64_t) override;
  virtual bool
  ProcessInput(uint64_t, BYTE *, std::function<void(const views::Editing &)>,
               std::function<void(const views::Converting &)>,
               std::function<void(const views::Committed &)>) override;
  virtual void ToggleInputMode() override;
  virtual InputMode GetInputMode() override;

private:
  StatefulIMEProxy(Connection *);

  Connection *conn_;

public:
  static StatefulIMEProxy *CreateIPCPRoxy(const WCHAR *const named_pipe_path);
  static StatefulIMEProxy *CreateTCPPRoxy(const std::string &host,
                                          const std::string &port);
};

} // namespace im
} // namespace win
} // namespace senn
