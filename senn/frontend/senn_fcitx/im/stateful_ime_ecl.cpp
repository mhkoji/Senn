#include "stateful_ime_ecl.h"
#include "stateful_ime_proxy_ipc_json.h"
#include <cstring>
#include <sstream>

extern "C" {
void init_senn(cl_object);
}

namespace {

void EclToString(cl_object obj, std::string *str) {
  struct ecl_string ecl_str = obj->string;
  for (int i = 0; i < ecl_str.fillp; i += 1) {
    *str += (typeof(ecl_str.elttype))ecl_str.self[i];
  }
}

} // namespace

namespace senn {
namespace fcitx {
namespace im {

void StatefulIMEEcl::ClBoot() {
  char ecl_str[16];
  strncpy(ecl_str, "ecl", sizeof(ecl_str));
  char *ecl[1] = {ecl_str};
  cl_boot(1, ecl);
}

void StatefulIMEEcl::EclInitModule() { ecl_init_module(NULL, init_senn); }

void StatefulIMEEcl::ClShutdown() { cl_shutdown(); }

StatefulIMEEcl *StatefulIMEEcl::Create() {
  cl_object ime = cl_eval(c_string_to_object("(senn.bin.fcitx-lib:make-ime)"));
  return new StatefulIMEEcl(ime);
}

StatefulIMEEcl::StatefulIMEEcl(cl_object ime) : ime_(ime) {}

StatefulIMEEcl::~StatefulIMEEcl() {}

void StatefulIMEEcl::ResetIM() {
  std::string response = "";
  EclToString(
      cl_funcall(2, cl_eval(c_string_to_object("'senn.bin.fcitx-lib:reset-im")),
                 ime_),
      &response);
  assert(response == "OK");
}

bool StatefulIMEEcl::SelectCandidate(
    int index,
    std::function<void(const senn::fcitx::im::views::Converting *)> on_conv,
    std::function<void(const senn::fcitx::im::views::Editing *)> on_editing) {
  std::string response = "";
  EclToString(cl_funcall(3,
                         cl_eval(c_string_to_object(
                             "'senn.bin.fcitx-lib:select-candidate")),
                         ime_, ecl_make_fixnum(index)),
              &response);

  std::istringstream iss(response);
  bool consumed;
  std::string type;
  iss >> consumed >> type;

  if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Converting v;
    senn::fcitx::im::stateful_ime_proxy_ipc_json::Parse(content, &v);
    on_conv(&v);
  } else if (type == "EDITING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Editing v;
    senn::fcitx::im::stateful_ime_proxy_ipc_json::Parse(content, &v);
    on_editing(&v);
  }

  return consumed;
}

bool StatefulIMEEcl::ProcessInput(
    uint32_t sym, uint32_t keycode, uint32_t state,
    std::function<void(const senn::fcitx::im::views::Converting *)> on_conv,
    std::function<void(const senn::fcitx::im::views::Editing *)> on_editing) {
  std::string response = "";
  EclToString(
      cl_funcall(
          4, cl_eval(c_string_to_object("'senn.bin.fcitx-lib:process-input")),
          ime_, ecl_make_fixnum(sym), ecl_make_fixnum(state)),
      &response);

  std::istringstream iss(response);
  bool consumed;
  std::string type;
  iss >> consumed >> type;

  if (type == "CONVERTING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Converting v;
    senn::fcitx::im::stateful_ime_proxy_ipc_json::Parse(content, &v);
    on_conv(&v);
  } else if (type == "EDITING") {
    std::string content;
    std::getline(iss, content);

    senn::fcitx::im::views::Editing v;
    senn::fcitx::im::stateful_ime_proxy_ipc_json::Parse(content, &v);
    on_editing(&v);
  }

  return consumed;
}

} // namespace im
} // namespace fcitx
} // namespace senn
