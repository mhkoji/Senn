#pragma once

#include <msctf.h>
#include <string>

#include "../../ime/views.h"
#include "candidate_window.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

class CandidateListUI : public ITfCandidateListUIElement,
                        public ITfTextLayoutSink {
public:
  class Handlers {
  public:
    virtual HRESULT OnLayoutChange(ITfContext *, ITfContextView *) = 0;
  };

  CandidateListUI(ITfContext *context, ITfThreadMgr *thread_mgr,
                  candidate_window::View *view)
      : ref_count_(1), context_(context), thread_mgr_(thread_mgr),
        ui_element_id_(-1), hwnd_(nullptr), view_(view),
        text_layout_sink_cookie_(TF_INVALID_COOKIE), handlers_(nullptr) {
    context_->AddRef();
    thread_mgr_->AddRef();
  }

  ~CandidateListUI() {
    context_->Release();
    thread_mgr_->Release();
  }

  void NotifyUpdateUI();

  void Move(RECT *);

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

  // ITfTextLayoutSink
  virtual HRESULT __stdcall OnLayoutChange(ITfContext *pic, TfLayoutCode lcode,
                                           ITfContextView *pView) override;

  static CandidateListUI *Create(ITfContext *context, ITfThreadMgr *thread_mgr,
                                 candidate_window::View *, Handlers *);

  void DestroyUI();

private:
  ULONG ref_count_;

  ITfContext *context_;

  ITfThreadMgr *thread_mgr_;

  DWORD ui_element_id_;

  HWND hwnd_;

  candidate_window::View *view_;

  DWORD text_layout_sink_cookie_;

  Handlers *handlers_;
};

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
