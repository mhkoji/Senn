#include <msctf.h>
#include <string>

#include "../../senn.h"
#include "../../variable.h"
#include "candidate_ui_list.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

const size_t kPageSize = 9;

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

  BSTR bstrDesc = SysAllocString(L"Senn Candidate List UI");

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
  if (hwnd_) {
    // If SW_SHOW is used instead of SW_SHOWNA, the window hangs.
    ShowWindow(hwnd_, bShow ? SW_SHOWNA : SW_HIDE);
  }
  return S_OK;
}

HRESULT __stdcall CandidateListUI::IsShown(BOOL *pbShow) {
  if (pbShow == nullptr) {
    return E_INVALIDARG;
  }
  *pbShow = hwnd_ && IsWindowVisible(hwnd_);
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

  NotifyUpdateUI();
}

void CandidateListUI::ClearCandidates() {
  current_index_ = 0;
  candidates_.clear();

  NotifyUpdateUI();
}

void CandidateListUI::NotifyUpdateUI() {
  if (hwnd_) {
    // Send WM_PAINT message
    InvalidateRect(hwnd_, nullptr, true);
    UpdateWindow(hwnd_);
  } else {
    ITfUIElementMgr *ui_mgr = nullptr;
    if (thread_mgr_->QueryInterface(IID_ITfUIElementMgr, (void **)&ui_mgr) ==
            S_OK &&
        ui_mgr != nullptr) {
      ui_mgr->UpdateUIElement(ui_element_id_);
      ui_mgr->Release();
    }
  }
}

void CandidateListUI::DestroyUI() {
  if (hwnd_) {
    DestroyWindow(hwnd_);
  } else {
    ITfUIElementMgr *ui_mgr = nullptr;
    if (thread_mgr_->QueryInterface(IID_ITfUIElementMgr, (void **)&ui_mgr) ==
            S_OK &&
        ui_mgr != nullptr) {
      ui_mgr->EndUIElement(ui_element_id_);
      ui_mgr->Release();
    }
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

  CandidateListUI *candidate_list_ui = new CandidateListUI(context, thread_mgr);
  BOOL tip_should_show_window = true;
  ui_mgr->BeginUIElement(candidate_list_ui, &tip_should_show_window,
                         &candidate_list_ui->ui_element_id_);

  if (tip_should_show_window) {
    ui_mgr->EndUIElement(candidate_list_ui->ui_element_id_);

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

HRESULT __stdcall CandidateListUI::GetUpdatedFlags(DWORD *pdwFlags) {
  if (pdwFlags == nullptr) {
    return E_INVALIDARG;
  }
  *pdwFlags = 0;
  *pdwFlags |= (TF_CLUIE_STRING | TF_CLUIE_COUNT);
  *pdwFlags |= (TF_CLUIE_SELECTION | TF_CLUIE_CURRENTPAGE | TF_CLUIE_PAGEINDEX);
  return S_OK;
}

HRESULT __stdcall CandidateListUI::GetDocumentMgr(ITfDocumentMgr **ppdim) {
  if (ppdim == nullptr) {
    return E_INVALIDARG;
  }
  return context_->GetDocumentMgr(ppdim);
}

HRESULT __stdcall CandidateListUI::GetCount(UINT *puCount) {
  if (puCount == nullptr) {
    return E_INVALIDARG;
  }
  *puCount = candidates_.size();
  return S_OK;
}

HRESULT __stdcall CandidateListUI::GetSelection(UINT *puIndex) {
  if (puIndex == nullptr) {
    return E_INVALIDARG;
  }
  *puIndex = current_index_;
  return S_OK;
}

HRESULT __stdcall CandidateListUI::GetString(UINT uIndex, BSTR *pstr) {
  if (pstr == nullptr) {
    return E_INVALIDARG;
  }
  if (candidates_.size() <= uIndex) {
    return E_INVALIDARG;
  }
  *pstr = SysAllocString(candidates_[uIndex].c_str());
  return S_OK;
}

HRESULT __stdcall CandidateListUI::GetPageIndex(UINT *pIndex, UINT uSize,
                                                UINT *puPageCnt) {
  if (puPageCnt == nullptr) {
    return E_INVALIDARG;
  }

  *puPageCnt = (candidates_.size() / kPageSize) + 1;

  if (pIndex == nullptr) {
    // https://docs.microsoft.com/ja-jp/windows/win32/api/msctf/nf-msctf-itfcandidatelistuielement-getpageindex
    // > The caller calls this method with NULL for this parameter first to get
    // the number of pages in puPageCnt and allocates the buffer to receive
    // indexes for all pages.
    return S_OK;
  }

  if (uSize < *puPageCnt) {
    return E_NOT_SUFFICIENT_BUFFER;
  }

  for (size_t i = 0; i < *puPageCnt; i++) {
    pIndex[i] = i * kPageSize;
  }

  return S_OK;
}

HRESULT __stdcall CandidateListUI::SetPageIndex(UINT *pIndex, UINT uPageCnt) {
  return E_NOTIMPL;
}

HRESULT __stdcall CandidateListUI::GetCurrentPage(UINT *puPage) {
  if (puPage == nullptr) {
    return E_INVALIDARG;
  }
  *puPage = current_index_ / kPageSize;
  return S_OK;
}

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn