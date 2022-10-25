#include <algorithm>
#include <msctf.h>
#include <string>

#include "../senn.h"
#include "candidate_window.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace candidate_window {

bool RegisterWindowClass(HINSTANCE hInst) {
  WNDCLASSEXW wc = {};

  wc.cbSize = sizeof(wc);
  wc.style = CS_VREDRAW | CS_HREDRAW;
  wc.lpfnWndProc = WindowProc;
  wc.cbClsExtra = 0;
  wc.cbWndExtra = 0;
  wc.hInstance = hInst;
  wc.hIcon = nullptr;
  wc.hCursor = LoadCursorW(nullptr, IDC_ARROW);
  wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1);
  wc.lpszMenuName = nullptr;
  wc.lpszClassName = senn::senn_win::kCandidateWindowClassName;
  wc.hIconSm = nullptr;

  ATOM atom = RegisterClassExW(&wc);
  return atom != 0;
}

void UnregisterWindowClass(HINSTANCE hInst) {
  UnregisterClassW(senn::senn_win::kCandidateWindowClassName, hInst);
}

namespace {

const LONG kMarginX = 20;
const LONG kMarginY = 10;

const UINT kDTFormat =
    DT_NOCLIP | DT_NOPREFIX | DT_LEFT | DT_SINGLELINE | DT_NOFULLWIDTHCHARBREAK;

void DrawCandidates(HDC hdc, const View *view, HBRUSH hbrHighlight,
                    LONG area_width) {
  const std::vector<std::wstring> *candidates = view->candidates();

  const int current_page = view->current_index() / kPageSize;

  const int begin_index = current_page * kPageSize;

  const int end_index = (std::min)((current_page + 1) * kPageSize,
                                   static_cast<int>(view->candidate_count()));

  SetBkMode(hdc, TRANSPARENT);

  LONG prev_bottom = 0;
  for (int index = begin_index; index < end_index; ++index) {
    const std::wstring &s = (*view->candidates())[index];

    RECT r_temp = {0, 0, 0, 0};
    DrawText(hdc, s.c_str(), -1, &r_temp, DT_CALCRECT | kDTFormat);
    LONG text_height = r_temp.bottom;

    LONG top = prev_bottom;
    LONG bottom = top + kMarginY + text_height + kMarginY;

    if (index == view->current_index()) {
      RECT r = {0, top, area_width, bottom};
      FillRect(hdc, &r, hbrHighlight);
    }

    {
      RECT r = {kMarginX, top, area_width, bottom};
      DrawText(hdc, s.c_str(), -1, &r, DT_VCENTER | kDTFormat);
    }

    prev_bottom = bottom;
  }
}

} // namespace

LRESULT CALLBACK WindowProc(HWND hwnd, UINT umsg, WPARAM wparam,
                            LPARAM lparam) {
  static HBRUSH hbrHighlight;

  View *view;
  if (umsg == WM_NCCREATE) {
    view = (View *)((LPCREATESTRUCTW(lparam))->lpCreateParams);
    SetWindowLongPtrW(hwnd, GWLP_USERDATA, LONG_PTR(view));
  } else {
    view = (View *)(GetWindowLongPtrW(hwnd, GWLP_USERDATA));
  }

  switch (umsg) {
  case WM_CREATE:
    hbrHighlight = CreateSolidBrush(RGB(0x97, 0xC2, 0xE2));
    return 0;
  case WM_DESTROY:
    DeleteObject(hbrHighlight);
    return 0;

  case WM_PAINT: {
    PAINTSTRUCT ps;
    HDC hdc = BeginPaint(hwnd, &ps);

    LONG width = 0, height = 0;
    {
      HDC hdcmem = CreateCompatibleDC(hdc);
      CalculateSize(hdcmem, view, &width, &height);
      DeleteDC(hdcmem);
    }

    HDC hdcmem = CreateCompatibleDC(hdc);
    RECT rc = {0, 0, width, height};
    HBITMAP hbmpmem = CreateCompatibleBitmap(hdc, rc.right, rc.bottom);
    SelectObject(hdcmem, hbmpmem);
    FillRect(hdcmem, &rc, HBRUSH(GetStockObject(WHITE_BRUSH)));

    DrawCandidates(hdcmem, view, hbrHighlight, width);
    BitBlt(hdc, 0, 0, width, height, hdcmem, 0, 0, SRCCOPY);

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

void CalculateSize(HDC hdc, const View *view, LONG *out_width,
                   LONG *out_height) {
  const std::vector<std::wstring> *candidates = view->candidates();

  const UINT current_page = view->current_index() / kPageSize;

  const UINT begin_index = current_page * kPageSize;

  const UINT end_index =
      (std::min)((current_page + 1) * kPageSize, view->candidate_count());

  LONG max_text_width = 0;
  LONG prev_bottom = 0;
  for (UINT index = begin_index; index < end_index; ++index) {
    const std::wstring &s = (*view->candidates())[index];

    RECT r_temp = {0, 0, 0, 0};
    DrawText(hdc, s.c_str(), -1, &r_temp, DT_CALCRECT | kDTFormat);
    LONG text_width = r_temp.right;
    LONG text_height = r_temp.bottom;

    LONG top = prev_bottom;
    LONG bottom = top + kMarginY + text_height + kMarginY;

    prev_bottom = bottom;

    if (max_text_width < text_width) {
      max_text_width = text_width;
    }
  }

  *out_width = kMarginX + max_text_width + kMarginX;
  *out_height = prev_bottom;
}

} // namespace candidate_window
} // namespace text_service
} // namespace senn_win
} // namespace senn
