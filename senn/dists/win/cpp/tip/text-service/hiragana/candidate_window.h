#pragma once

#include <msctf.h>

#include "../../ime/views.h"
#include <string>

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

class CandidateWindow : public ITfUIElement {
public:
  CandidateWindow(ITfThreadMgr *thread_mgr)
      : thread_mgr_(thread_mgr), ref_count_(1), hwnd_(nullptr), shown_(false),
        tip_should_show_(false), ui_element_id_(-1),
        candidates_(std::vector<std::wstring>()) {
    thread_mgr_->AddRef();
  }

  ~CandidateWindow() { thread_mgr_->Release(); }

  // ITfUIElement
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override;

  virtual ULONG __stdcall AddRef(void) override;

  virtual ULONG __stdcall Release(void) override;

  virtual HRESULT __stdcall GetDescription(BSTR *pbstrDescription) override;

  virtual HRESULT __stdcall GetGUID(GUID *pguid) override;

  virtual HRESULT __stdcall Show(BOOL bShow) override;

  virtual HRESULT __stdcall IsShown(BOOL *pbShow) override;

  void ShowCandidates(const senn::senn_win::ime::views::Converting &);

  static LRESULT CALLBACK WindowProc(HWND hwnd, UINT uMsg, WPARAM wparam,
                                     LPARAM lparam);

  static CandidateWindow *Create(ITfContext *context, ITfThreadMgr *thread_mgr);

  static bool RegisterWindowClass();

  static void UnregisterWindowClass();

private:
  ITfThreadMgr *thread_mgr_;

  ULONG ref_count_;

  HWND hwnd_;

  BOOL shown_;

  BOOL tip_should_show_;

  DWORD ui_element_id_;

  std::vector<std::wstring> candidates_;
};

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn
