#pragma once
#include "request.h"
#include "stateful_ime.h"

extern "C" {

typedef void *ime_t;
typedef enum {
  SENN_ERR_SUCCESS = 0,
  SENN_ERR_FAIL = 1,
} err_t;

extern err_t (*senn_make_ime)(ime_t *result);
extern err_t (*senn_close_ime)(ime_t *result);
extern err_t (*senn_handle_request)(ime_t ime, char *req, char **result);

extern void (*release_handle)(void *handle);
extern int senn_init(const char *);
}

namespace senn {
namespace ibus {
namespace im {

class StatefulIMESbcl {
private:
  class Requester : public senn::RequesterInterface {
  public:
    Requester(ime_t);
    ~Requester();

    void Request(const std::string &, std::string *) override;

  private:
    ime_t ime_;
  };

public:
  static int Init(const char[]);
  static StatefulIME *Create();
}; // namespace im

} // namespace im
} // namespace ibus
} // namespace senn
