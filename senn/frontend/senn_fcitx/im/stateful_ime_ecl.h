#pragma once
#include "ipc/request.h"
#include "stateful_ime.h"
#include "views.h"
#include <ecl/ecl.h>
#include <memory>

namespace senn {
namespace fcitx {
namespace im {

class StatefulIMEEcl {
private:
  class Requester : public senn::ipc::RequesterInterface {
  public:
    Requester(cl_object);
    ~Requester();

    void Request(const std::string &, std::string *) override;

  private:
    cl_object ime_;
  };

public:
  static void ClBoot();
  static void EclInitModule();
  static void ClShutdown();
  static StatefulIME *Create();
}; // namespace im

} // namespace im
} // namespace fcitx
} // namespace senn
