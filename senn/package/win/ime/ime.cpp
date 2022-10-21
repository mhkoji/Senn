#include "ime.h"
#include "variable.h"
#include <ecl/ecl.h>

void EclToString(cl_object obj, std::string *str) {
  struct ecl_string ecl_str = obj->string;
  for (int i = 0; i < ecl_str.fillp; i += 1) {
    *str += (decltype(ecl_str.elttype))ecl_str.self[i];
  }
}

class IME::Impl {
public:
  Impl() {
    ime_ = cl_funcall(2, cl_eval(c_string_to_object("'senn.lib.win:make-ime")),
                      ecl_make_constant_base_string(kKkcEnginePath, -1));
  }

  ~Impl() {
    cl_funcall(2, cl_eval(c_string_to_object("'senn.lib.win:close-ime")), ime_);
  }

  void HandleRequest(const std::string &req, std::string *res) {
    // std::cout << req << std::endl;
    cl_object response = cl_funcall(
        3, cl_eval(c_string_to_object("'senn.lib.win:handle-request")), ime_,
        ecl_make_constant_base_string(req.c_str(), -1));
    EclToString(response, res);
  }

private:
  cl_object ime_;
};

IME::IME() : impl_(new IME::Impl()) {}

IME::~IME() { delete impl_; }

void IME::HandleRequest(const std::string &req, std::string *res) {
  impl_->HandleRequest(req, res);
}
