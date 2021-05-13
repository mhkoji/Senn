#include <algorithm>
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

const UINT format = DT_NOCLIP | DT_NOPREFIX | DT_SINGLELINE | DT_WORDBREAK |
                    DT_NOFULLWIDTHCHARBREAK;

void DrawCandidates(HDC hdc, const CandidateWindow::View *view) {
  const std::vector<std::wstring> *candidates = view->candidates();

  const size_t current_page =
      view->current_index() / CandidateWindow::kPageSize;

  const size_t begin_index = current_page * CandidateWindow::kPageSize;

  const size_t end_index = (std::min)(
      (current_page + 1) * CandidateWindow::kPageSize, candidates->size());

  LONG prev_bottom = 0;
  for (size_t index = begin_index; index < end_index; ++index) {
    const std::wstring &s = (*view->candidates())[index];

    RECT r_temp = {0, 0, 1, 1};
    DrawText(hdc, s.c_str(), -1, &r_temp, DT_CALCRECT | format);

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
    DrawText(hdc, s.c_str(), -1, &r, format);

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
  case WM_DESTROY:
    delete cw;
    return 0;
  case WM_PAINT: {
    PAINTSTRUCT ps;
    HDC hdc = BeginPaint(hwnd, &ps);
    HDC hdcmem = CreateCompatibleDC(hdc);
    RECT rc = {0, 0, 100, 500};
    HBITMAP hbmpmem = CreateCompatibleBitmap(hdc, rc.right, rc.bottom);
    SelectObject(hdcmem, hbmpmem);
    FillRect(hdcmem, &rc, HBRUSH(GetStockObject(WHITE_BRUSH)));

    DrawCandidates(hdcmem, cw->view_);

    BitBlt(hdc, 0, 0, 100, 500, hdcmem, 0, 0, SRCCOPY);
    DeleteObject(hbmpmem);
    DeleteDC(hdcmem);
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
