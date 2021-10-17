#pragma once

#include <windows.h>
#include <winsock2.h>

#include "stateful_im.h"

namespace senn {
namespace senn_win {
namespace ime {

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

class StatefulIMProxy : public StatefulIM {
public:
  bool CanProcess(uint64_t) override;
  
  bool ProcessInput(uint64_t,
                    std::function<void(const views::Editing &)>,
                    std::function<void(const views::Converting &)>,
                    std::function<void(const views::Committed &)>) override;



  ~StatefulIMProxy() override;

private:
  StatefulIMProxy(Connection *);

  Connection *conn_;

public:
  static StatefulIMProxy *CreateIPCPRoxy(const WCHAR *const named_pipe_path);
  static StatefulIMProxy *CreateTCPPRoxy(const std::string &host,
                                         const std::string &port);
};

} // namespace ime
} // namespace senn_win
} // namespace senn
