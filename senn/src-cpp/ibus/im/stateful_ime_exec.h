#pragma once
#include "request.h"
#include "stateful_ime.h"
#include <memory>

namespace senn {
namespace ibus {
namespace im {

class StatefulIMEExec {
private:
  class Requester : public senn::RequesterInterface {
  public:
    Requester(pid_t, int, int);
    ~Requester();

    void Request(const std::string &, std::string *) override;

  private:
    pid_t pid_;
    int fd_read_, fd_write_;
  };

public:
  static StatefulIME *Create();
}; // namespace im

} // namespace im
} // namespace ibus
} // namespace senn
