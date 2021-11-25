#pragma once
#include "stateful_ime.h"
#include "stateful_ime_proxy_ipc_json.h"
#include "views.h"
#include <ecl/ecl.h>
#include <memory>

namespace senn {
namespace fcitx {
namespace im {

class StatefulIMEEcl : public StatefulIME {
public:
  ~StatefulIMEEcl();

  void ResetIM() override;

  bool SelectCandidate(
      int index,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) override;

  bool ProcessInput(
      uint32_t, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) override;

private:
  cl_object ime_;

  StatefulIMEEcl(cl_object);

public:
  static void ClBoot();
  static void EclInitModule();
  static void ClShutdown();
  static StatefulIMEEcl *Create();
};

} // namespace im
} // namespace fcitx
} // namespace senn
