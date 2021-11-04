#pragma once
#include <memory>
#include <string>

#include "ipc.h"

namespace senn {
namespace ipc {

class RequesterInterface {
public:
  virtual ~RequesterInterface() {};

  virtual void Request(const std::string&, std::string*) = 0;
};


class Requester : public RequesterInterface {
public:
  Requester(ConnectionFactory*);
  ~Requester();

  void Request(const std::string&, std::string*);

private:
  ConnectionFactory* const factory_;
  Connection *conn_;
};

} // ipc
} // senn
