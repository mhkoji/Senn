#include <msctf.h>

#include "../../senn.h"
#include "../../variable.h"
#include "candidate_window.h"
#include <string>

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

HRESULT __stdcall CandidateWindow::QueryInterface(REFIID riid,
                                                  void **ppvObject) {
  if (ppvObject == nullptr) {
    return E_INVALIDARG;
  }

  *ppvObject = nullptr;

  if (IsEqualIID(riid, IID_IUnknown) || IsEqualIID(riid, IID_ITfUIElement)) {
    *ppvObject = static_cast<ITfUIElement *>(this);
  }

  if (*ppvObject) {
    AddRef();
    return S_OK;
  }

  return E_NOINTERFACE;
}

ULONG __stdcall CandidateWindow::AddRef(void) { return ++ref_count_; }

ULONG __stdcall CandidateWindow::Release(void) {
  if (ref_count_ <= 0) {
    return 0;
  }

  const ULONG count = --ref_count_;
  if (count == 0) {
    delete this;
  }
  return count;
}

HRESULT __stdcall CandidateWindow::GetDescription(BSTR *pbstrDescription) {
  if (pbstrDescription == nullptr) {
    return E_INVALIDARG;
  }

  *pbstrDescription = nullptr;

  BSTR bstrDesc = SysAllocString(L"Senn");

  if (bstrDesc == nullptr) {
    return E_OUTOFMEMORY;
  }

  *pbstrDescription = bstrDesc;

  return S_OK;
}

// {68CC0134-855F-4216-A6EB-7D568BB808C2}
static const GUID guid_CANDIDATE_WINDOW = {
    0x68cc0134,
    0x855f,
    0x4216,
    {0xa6, 0xeb, 0x7d, 0x56, 0x8b, 0xb8, 0x8, 0xc2}};

HRESULT __stdcall CandidateWindow::GetGUID(GUID *pguid) {
  if (pguid == nullptr) {
    return E_INVALIDARG;
  }
  *pguid = guid_CANDIDATE_WINDOW;
  return S_OK;
}

HRESULT __stdcall CandidateWindow::Show(BOOL bShow) {
  if (bShow) {
    ShowWindow(hwnd_, SW_SHOW);
  } else {
    ShowWindow(hwnd_, SW_HIDE);
  }
  shown_ = bShow;
  return S_OK;
}

HRESULT __stdcall CandidateWindow::IsShown(BOOL *pbShow) {
  if (pbShow == nullptr) {
    return E_FAIL;
  }
  *pbShow = shown_;
  return S_OK;
}

void CandidateWindow::ShowCandidates(
    const senn::senn_win::ime::views::Converting &view) {
  candidates_.clear();
  for (std::vector<std::wstring>::const_iterator it =
           view.cursor_form_candidates.begin();
       it != view.cursor_form_candidates.end(); ++it) {
    candidates_.push_back(*it);
  }
  // Send WM_PAINT message
  InvalidateRect(hwnd_, nullptr, true);
  UpdateWindow(hwnd_);
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
    HDC hdc = BeginPaint(cw->hwnd_, &ps);
    for (size_t i = 0; i < cw->candidates_.size(); ++i) {
      std::wstring &s = cw->candidates_[i];
      TextOut(hdc, 0, int(i * 50), s.c_str(), s.length());
    }

    EndPaint(cw->hwnd_, &ps);
    return 0;
  }
  default:
    break;
  }
  return DefWindowProc(hwnd, umsg, wparam, lparam);
}

// https://docs.microsoft.com/en-us/windows/win32/tsf/uiless-mode-overview#the-flow-chart-of-uilessmode
CandidateWindow *CandidateWindow::Create(ITfContext *context,
                                         ITfThreadMgr *thread_mgr) {

  ITfUIElementMgr *ui_mgr = nullptr;
  if (thread_mgr->QueryInterface(IID_ITfUIElementMgr, (void **)&ui_mgr) ==
      S_OK) {
    if (ui_mgr == nullptr) {
      return nullptr;
    }
  }

  CandidateWindow *cw = new CandidateWindow(thread_mgr);

  ui_mgr->BeginUIElement(cw, &cw->tip_should_show_, &cw->ui_element_id_);

  if (cw->tip_should_show_) {

    HWND hwndParent = nullptr;
    {
      ITfContextView *pView;
      if (context->GetActiveView(&pView) == S_OK) {
        if (FAILED(pView->GetWnd(&hwndParent)) || (hwndParent == nullptr)) {
          hwndParent = GetFocus();
        }
      } else {
        hwndParent = GetFocus();
      }
    }

    cw->hwnd_ = ::CreateWindowExW(WS_EX_TOOLWINDOW | WS_EX_TOPMOST,
                                  senn::senn_win::kSennCandidateWindowClassName,
                                  L"", WS_POPUP | WS_BORDER, 50, 50, 100, 500,
                                  hwndParent, nullptr, g_module_handle, cw);
    ShowWindow(cw->hwnd_, SW_SHOWNA);
  } else {
    ui_mgr->UpdateUIElement(cw->ui_element_id_);
  }

  ui_mgr->Release();

  return cw;
}

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

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
