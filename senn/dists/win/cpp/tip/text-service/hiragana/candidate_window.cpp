#include <msctf.h>
#include <string>

#include "../../senn.h"
#include "../../variable.h"
#include "candidate_window.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

CandidateWindow::CandidateWindow(View *view) : view_(view) {}

bool CandidateWindow::RegisterWindowClass() {
  WNDCLASSEXW wc = {};

  wc.cbSize = sizeof(wc);
  wc.style = CS_VREDRAW | CS_HREDRAW;
  wc.lpfnWndProc = CandidateWindow::WindowProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = g_module_handle;
  wc.hIcon = nullptr;
  wc.hCursor = LoadCursorW(nullptr, IDC_ARROW);
  wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
  wc.lpszMenuName = nullptr;
  wc.lpszClassName = senn::senn_win::kSennCandidateWindowClassName;
  wc.hIconSm = nullptr;

  ATOM atom = RegisterClassExW(&wc);
  return atom != 0;
}

void CandidateWindow::UnregisterWindowClass() {
  UnregisterClassW(senn::senn_win::kSennCandidateWindowClassName,
                   g_module_handle);
}

namespace {

const LONG MARGIN_X = 2;
const LONG MARGIN_Y = 4;

void DrawCandidates(HDC hdc, const CandidateWindow::View *view) {
  UINT format = DT_NOCLIP | DT_NOPREFIX | DT_SINGLELINE | DT_WORDBREAK |
                DT_NOFULLWIDTHCHARBREAK;
  const std::vector<std::wstring> *candidates = view->candidates();
  std::vector<std::wstring>::const_iterator it = candidates->begin();
  LONG prev_bottom = 0;
  for (size_t index = 0; it != candidates->end(); ++it, ++index) {
    RECT r_temp = {0, 0, 1, 1};
    DrawText(hdc, it->c_str(), -1, &r_temp, DT_CALCRECT | format);

    if (index == view->current_index()) {
      // Set highlight
      SetBkColor(hdc, RGB(0x97, 0xC2, 0xE2));
      SetBkMode(hdc, OPAQUE);
    } else {
      SetBkMode(hdc, TRANSPARENT);
    }

    LONG width = r_temp.right;
    LONG height = r_temp.bottom;

    LONG top = prev_bottom + MARGIN_Y;
    LONG bottom = top + height + MARGIN_Y;
    LONG left = MARGIN_X;
    LONG right = left + width + MARGIN_X;
    RECT r = {left, top, right, bottom};
    DrawText(hdc, it->c_str(), -1, &r, format);

    prev_bottom = bottom;
  }
}

} // namespace

LRESULT CALLBACK CandidateWindow::WindowProc(HWND hwnd, UINT umsg,
                                             WPARAM wparam, LPARAM lparam) {
  CandidateWindow *cw;
  if (umsg == WM_NCCREATE) {
    cw = (CandidateWindow *)((LPCREATESTRUCTW(lparam))->lpCreateParams);
    SetWindowLongPtrW(hwnd, GWLP_USERDATA, LONG_PTR(cw));
  } else {
    cw = (CandidateWindow *)(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
  }

  switch (umsg) {
  case WM_PAINT: {
    PAINTSTRUCT ps;
    HDC hdc = BeginPaint(hwnd, &ps);
    DrawCandidates(hdc, cw->view_);
    EndPaint(hwnd, &ps);
    return 0;
  }
  default:
    break;
  }
  return DefWindowProc(hwnd, umsg, wparam, lparam);
}

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
