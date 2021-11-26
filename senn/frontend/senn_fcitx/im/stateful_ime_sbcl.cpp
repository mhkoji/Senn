#include "stateful_ime_sbcl.h"
#include "stateful_ime_proxy_ipc.h"
#include <cstring>

namespace senn {
namespace fcitx {
namespace im {

StatefulIMESbcl::Requester::Requester(ime_t ime) : ime_(ime) {}

StatefulIMESbcl::Requester::~Requester() {}

void StatefulIMESbcl::Requester::Request(const std::string &req,
                                         std::string *res) {
  char *buf = nullptr;
  senn_handle_request(ime_, (char *)(req.c_str()), &buf);
  if (buf) {
    *res = std::string(buf);
  }
}

int StatefulIMESbcl::Init(const char* core) { return senn_init(core); }

StatefulIME *StatefulIMESbcl::Create() {
  ime_t ime = nullptr;
  senn_make_ime(&ime);
  return new StatefulIMEProxyIPC(std::unique_ptr<senn::ipc::RequesterInterface>(
      new StatefulIMESbcl::Requester(ime)));
}

} // namespace im
} // namespace fcitx
} // namespace senn
