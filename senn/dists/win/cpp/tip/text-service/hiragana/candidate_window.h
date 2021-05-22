#pragma once

#include <msctf.h>
#include <string>

#include "../../ime/views.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

class CandidateWindow {
public:
  static const UINT kPageSize = 9;

  class View {
  public:
    virtual const std::vector<std::wstring> *candidates() const = 0;

    virtual UINT current_index() const = 0;

    UINT candidate_count() const {
      return static_cast<UINT>(candidates()->size());
    }
  };

  CandidateWindow(View *);

  static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wparam,
                                     LPARAM lparam);

  static bool RegisterWindowClass(HINSTANCE);

  static void UnregisterWindowClass(HINSTANCE);

private:
  View *view_;
};

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
