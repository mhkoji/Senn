#pragma once

#include <msctf.h>

#include "../../ime/views.h"
#include <string>

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

class CandidateWindow {
public:
  class View {
  public:
    virtual const std::vector<std::wstring>* candidates() const = 0;
    
    virtual size_t current_index() const = 0;
  };

  CandidateWindow(View *);

  static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wparam,
                                     LPARAM lparam);

  static bool RegisterWindowClass();

  static void UnregisterWindowClass();

private:
  View *view_;
};

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
