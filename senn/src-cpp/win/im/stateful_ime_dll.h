#pragma once

#include "../../../package/win/ime/ime.h"
#include "stateful_ime_proxy.h"

namespace senn {
namespace win {
namespace im {

class StatefulIMEDll {
  class Requester : public senn::RequesterInterface {
  public:
    Requester(IME *ime) : ime_(ime) {}
    ~Requester() { delete ime_; }

  private:
    IME *ime_;

    // RequesterInterface
    virtual void Request(const std::string &req, std::string *res) override {
      ime_->HandleRequest(req, res);
    }
  };

public:
  static StatefulIMEProxy *Create() {
    Requester *requester = new StatefulIMEDll::Requester(new IME());
    return new StatefulIMEProxy(
        std::unique_ptr<senn::RequesterInterface>(requester));
  }
};

} // namespace im
} // namespace win
} // namespace senn
