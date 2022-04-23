#pragma once

#include "../../request.h"
#include "stateful_ime.h"
#include <memory>

namespace senn {
namespace win {
namespace im {

class StatefulIMEProxy : public StatefulIME {
public:
  StatefulIMEProxy(std::unique_ptr<senn::RequesterInterface>);
  ~StatefulIMEProxy() override;

  bool CanProcess(uint64_t) override;
  bool ProcessInput(uint64_t, BYTE *,
                    std::function<void(const views::Editing &)>,
                    std::function<void(const views::Converting &)>,
                    std::function<void(const views::Committed &)>) override;
  void ToggleInputMode() override;
  InputMode GetInputMode() override;

private:
  std::unique_ptr<senn::RequesterInterface> requester_;
};

} // namespace im
} // namespace win
} // namespace senn
