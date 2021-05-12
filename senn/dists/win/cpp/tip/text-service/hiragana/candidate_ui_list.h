#pragma once

#include <msctf.h>
#include <string>

#include "../../ime/views.h"
#include "candidate_window.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

class CandidateListUI : public ITfUIElement, public CandidateWindow::View {
public:
  CandidateListUI(ITfThreadMgr *thread_mgr)
      : thread_mgr_(thread_mgr), ref_count_(1), shown_(false),
        should_show_original_window_(false), ui_element_id_(-1), hwnd_(nullptr),
        cw_(nullptr), candidates_(std::vector<std::wstring>()) {
    thread_mgr_->AddRef();
  }

  ~CandidateListUI() { thread_mgr_->Release(); }

  // ITfUIElement
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override;

  virtual ULONG __stdcall AddRef(void) override;

  virtual ULONG __stdcall Release(void) override;

  virtual HRESULT __stdcall GetDescription(BSTR *pbstrDescription) override;

  virtual HRESULT __stdcall GetGUID(GUID *pguid) override;

  virtual HRESULT __stdcall Show(BOOL bShow) override;

  virtual HRESULT __stdcall IsShown(BOOL *pbShow) override;

  // Candidate::State
  virtual const std::vector<std::wstring> *candidates() const override {
    return &candidates_;
  }

  virtual size_t current_index() const override {
    return current_index_;
  }

  void UpdateCandidates(const senn::senn_win::ime::views::Converting &);

  void ClearCandidates();

  static CandidateListUI *Create(ITfContext *context, ITfThreadMgr *thread_mgr);

private:
  ITfThreadMgr *thread_mgr_;

  ULONG ref_count_;

  BOOL shown_;

  BOOL should_show_original_window_;

  DWORD ui_element_id_;

  HWND hwnd_;

  CandidateWindow *cw_;

  std::vector<std::wstring> candidates_;

  size_t current_index_ = 0;
};

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
