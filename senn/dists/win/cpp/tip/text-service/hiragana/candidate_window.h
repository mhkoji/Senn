#pragma once

#include <msctf.h>

#include "../../ime/views.h"
#include <string>

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

class CandidateWindow {
public:
  class View {
  public:
    virtual const std::vector<std::wstring>* candidates() const = 0;
  };

  CandidateWindow(View *);

  static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wparam,
                                     LPARAM lparam);

  static bool RegisterWindowClass();

  static void UnregisterWindowClass();

private:
  View *view_;
};

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

  void ShowCandidates(const senn::senn_win::ime::views::Converting &);

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
};

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
