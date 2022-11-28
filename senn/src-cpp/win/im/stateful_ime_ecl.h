#pragma once
#include "../../request.h"
#include "stateful_ime.h"
#include <ecl/ecl.h>

namespace senn {
namespace win {
namespace im {

class StatefulIMEEcl {
private:
  class Requester : public senn::RequesterInterface {
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

  static void Init(const std::string &);
  static void Destroy();
  static StatefulIME *Create();
}; // namespace im

} // namespace im
} // namespace win
} // namespace senn
