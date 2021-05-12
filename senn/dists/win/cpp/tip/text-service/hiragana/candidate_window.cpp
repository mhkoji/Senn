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
    const std::vector<std::wstring> *candidates = cw->view_->candidates();
    for (size_t i = 0; i < candidates->size(); ++i) {
      const std::wstring &s = candidates->at(i);
      TextOut(hdc, 0, int(i * 50), s.c_str(), s.length());
    }

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
