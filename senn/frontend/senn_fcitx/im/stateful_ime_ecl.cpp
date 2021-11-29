#include "stateful_ime_ecl.h"
#include "stateful_ime_proxy.h"
#include <cstring>

extern "C" {
void init_senn(cl_object);
}

namespace {

void EclToString(cl_object obj, std::string *str) {
  struct ecl_string ecl_str = obj->string;
  for (int i = 0; i < ecl_str.fillp; i += 1) {
    *str += (__typeof(ecl_str.elttype))ecl_str.self[i];
  }
}

} // namespace

namespace senn {
namespace fcitx {
namespace im {

StatefulIMEEcl::Requester::Requester(cl_object ime) : ime_(ime) {}

StatefulIMEEcl::Requester::~Requester() {
  // TODO: StatefulIMEEcl should call close-ime
  cl_funcall(2, cl_eval(c_string_to_object("'senn.lib.fcitx:close-ime")), ime_);
}

void StatefulIMEEcl::Requester::Request(const std::string &req,
                                        std::string *res) {
  // std::cout << req << std::endl;
  cl_object response = cl_funcall(
      3, cl_eval(c_string_to_object("'senn.lib.fcitx:handle-request")), ime_,
      ecl_make_constant_base_string(req.c_str(), -1));
  EclToString(response, res);
}

void StatefulIMEEcl::ClBoot() {
  char ecl_str[16];
  strncpy(ecl_str, "ecl", sizeof(ecl_str));
  char *ecl[1] = {ecl_str};
  cl_boot(1, ecl);
}

void StatefulIMEEcl::EclInitModule() { ecl_init_module(NULL, init_senn); }

void StatefulIMEEcl::ClShutdown() { cl_shutdown(); }

StatefulIME *StatefulIMEEcl::Create() {
  cl_object ime = cl_eval(c_string_to_object("(senn.lib.fcitx:make-ime)"));
  return new StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>(
      new StatefulIMEEcl::Requester(ime)));
}

} // namespace im
} // namespace fcitx
} // namespace senn
