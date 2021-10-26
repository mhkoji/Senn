#pragma once

#include <msctf.h>
#include <string>

#include "../ime/views.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace candidate_window {

static const UINT kPageSize = 9;

class View {
public:
  virtual const std::vector<std::wstring> *candidates() const = 0;

  virtual UINT current_index() const = 0;

  UINT candidate_count() const {
    return static_cast<UINT>(candidates()->size());
  }
};

LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wparam, LPARAM lparam);

bool RegisterWindowClass(HINSTANCE);

void UnregisterWindowClass(HINSTANCE);

} // namespace candidate_window
} // namespace text_service
} // namespace senn_win
} // namespace senn
