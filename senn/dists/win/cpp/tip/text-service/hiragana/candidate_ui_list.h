#pragma once

#include <msctf.h>
#include <string>

#include "../../ime/views.h"
#include "candidate_window.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

class CandidateListUI : public ITfCandidateListUIElement {
public:
  CandidateListUI(ITfContext *context, ITfThreadMgr *thread_mgr,
                  CandidateWindow::View *view)
      : ref_count_(1), context_(context), thread_mgr_(thread_mgr),
        ui_element_id_(-1), hwnd_(nullptr), view_(view) {
    thread_mgr_->AddRef();
  }

  ~CandidateListUI() {
    DestroyUI();
    thread_mgr_->Release();
  }

  void NotifyUpdateUI();

  // ITfUIElement
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override;
  virtual ULONG __stdcall AddRef(void) override;
  virtual ULONG __stdcall Release(void) override;
  virtual HRESULT __stdcall GetDescription(BSTR *pbstrDescription) override;
  virtual HRESULT __stdcall GetGUID(GUID *pguid) override;
  virtual HRESULT __stdcall Show(BOOL bShow) override;
  virtual HRESULT __stdcall IsShown(BOOL *pbShow) override;

  // ITfCandidateListUIElement
  virtual HRESULT __stdcall GetUpdatedFlags(DWORD *pdwFlags) override;
  virtual HRESULT __stdcall GetDocumentMgr(ITfDocumentMgr **ppdim) override;
  virtual HRESULT __stdcall GetCount(UINT *puCount) override;
  virtual HRESULT __stdcall GetSelection(UINT *puIndex) override;
  virtual HRESULT __stdcall GetString(UINT uIndex, BSTR *pstr) override;
  virtual HRESULT __stdcall GetPageIndex(UINT *pIndex, UINT uSize,
                                         UINT *puPageCnt) override;
  virtual HRESULT __stdcall SetPageIndex(UINT *pIndex, UINT uPageCnt) override;
  virtual HRESULT __stdcall GetCurrentPage(UINT *puPage) override;

  static CandidateListUI *Create(ITfContext *context, ITfThreadMgr *thread_mgr,
                                 CandidateWindow::View *);

private:
  void DestroyUI();

  ULONG ref_count_;

  ITfContext *context_;

  ITfThreadMgr *thread_mgr_;

  DWORD ui_element_id_;

  HWND hwnd_;

  CandidateWindow::View *view_;
};

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
