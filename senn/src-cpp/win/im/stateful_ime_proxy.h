#pragma once

#include "stateful_ime.h"
#include <memory>

namespace senn {
namespace win {
namespace im {

class StatefulIMEProxy : public StatefulIME {
public:
  virtual void Request(const std::string &, std::string *) = 0;

  bool CanProcess(uint64_t) override;
  bool ProcessInput(uint64_t, BYTE *,
                    std::function<void(const views::Editing &)>,
                    std::function<void(const views::Converting &)>,
                    std::function<void(const views::Committed &)>) override;
  void ToggleInputMode() override;
  InputMode GetInputMode() override;
};

} // namespace im
} // namespace win
} // namespace senn
