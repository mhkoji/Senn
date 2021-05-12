#include <msctf.h>
#include <string>

#include "../../senn.h"
#include "../../variable.h"
#include "candidate_ui_list.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

HRESULT __stdcall CandidateListUI::QueryInterface(REFIID riid,
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

ULONG __stdcall CandidateListUI::AddRef(void) { return ++ref_count_; }

ULONG __stdcall CandidateListUI::Release(void) {
  if (ref_count_ <= 0) {
    return 0;
  }

  const ULONG count = --ref_count_;
  if (count == 0) {
    delete this;
  }
  return count;
}

HRESULT __stdcall CandidateListUI::GetDescription(BSTR *pbstrDescription) {
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
static const GUID guid_CANDIDATE_LIST_UI = {
    0x68cc0134,
    0x855f,
    0x4216,
    {0xa6, 0xeb, 0x7d, 0x56, 0x8b, 0xb8, 0x8, 0xc2}};

HRESULT __stdcall CandidateListUI::GetGUID(GUID *pguid) {
  if (pguid == nullptr) {
    return E_INVALIDARG;
  }
  *pguid = guid_CANDIDATE_LIST_UI;
  return S_OK;
}

HRESULT __stdcall CandidateListUI::Show(BOOL bShow) {
  if (bShow) {
    ShowWindow(hwnd_, SW_SHOWNA);
  } else {
    ShowWindow(hwnd_, SW_HIDE);
  }
  shown_ = bShow;
  return S_OK;
}

HRESULT __stdcall CandidateListUI::IsShown(BOOL *pbShow) {
  if (pbShow == nullptr) {
    return E_FAIL;
  }
  *pbShow = shown_;
  return S_OK;
}

void CandidateListUI::UpdateCandidates(
    const senn::senn_win::ime::views::Converting &view) {
  current_index_ = view.cursor_form_candidate_index;
  candidates_.clear();
  for (std::vector<std::wstring>::const_iterator it =
           view.cursor_form_candidates.begin();
       it != view.cursor_form_candidates.end(); ++it) {
    candidates_.push_back(*it);
  }

  if (should_show_original_window_) {
    // Send WM_PAINT message
    InvalidateRect(hwnd_, nullptr, true);
    UpdateWindow(hwnd_);
  }
}

void CandidateListUI::ClearCandidates() {
  current_index_ = 0;
  candidates_.clear();

  if (should_show_original_window_) {
    // Send WM_PAINT message
    InvalidateRect(hwnd_, nullptr, true);
    UpdateWindow(hwnd_);
  }
}


// https://docs.microsoft.com/en-us/windows/win32/tsf/uiless-mode-overview#the-flow-chart-of-uilessmode
CandidateListUI *CandidateListUI::Create(ITfContext *context,
                                         ITfThreadMgr *thread_mgr) {

  ITfUIElementMgr *ui_mgr = nullptr;
  if (thread_mgr->QueryInterface(IID_ITfUIElementMgr, (void **)&ui_mgr) ==
      S_OK) {
    if (ui_mgr == nullptr) {
      return nullptr;
    }
  }

  CandidateListUI *candidate_list_ui = new CandidateListUI(thread_mgr);

  ui_mgr->BeginUIElement(candidate_list_ui,
                         &candidate_list_ui->should_show_original_window_,
                         &candidate_list_ui->ui_element_id_);

  if (candidate_list_ui->should_show_original_window_) {
    CandidateWindow *cw = new CandidateWindow(
        static_cast<CandidateWindow::View *>(candidate_list_ui));

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

    candidate_list_ui->hwnd_ =
        CreateWindowEx(WS_EX_TOOLWINDOW | WS_EX_TOPMOST,
                       senn::senn_win::kSennCandidateWindowClassName, L"",
                       WS_POPUP | WS_BORDER, 50, 50, 100, 500, hwndParent,
                       nullptr, g_module_handle, cw);
  } else {
    ui_mgr->UpdateUIElement(candidate_list_ui->ui_element_id_);
  }

  ui_mgr->Release();

  return candidate_list_ui;
}

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn