#pragma once
#include "request.h"
#include "stateful_ime.h"
#include <memory>

namespace senn {
namespace ibus {
namespace im {

class StatefulIMEProxy : public StatefulIME {
public:
  StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>);
  ~StatefulIMEProxy();

  InputMode ToggleInputMode() override;

  bool SelectCandidate(
      int index,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) override;

  bool ProcessInput(
      uint32_t, uint32_t, uint32_t,
      std::function<void(const senn::fcitx::im::views::Converting *)>,
      std::function<void(const senn::fcitx::im::views::Editing *)>) override;

private:
  std::unique_ptr<senn::RequesterInterface> requester_;
};

} // namespace im
} // namespace ibus
} // namespace senn
