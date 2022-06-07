#pragma once
#pragma once

#include "candidate_ui_list.h"
#include <msctf.h>
#include <string>
#include <win/im/stateful_ime.h>

namespace senn {
namespace senn_win {
namespace text_service {

template <typename T> class Holder {
public:
  Holder() : ptr_(nullptr) {}

  T *Get() { return ptr_; }

  void Set(T *p) { ptr_ = p; }

private:
  T *ptr_;
};

typedef Holder<ITfComposition> CompositionHolder;

class CandidateList : public CandidateListUI::Handlers {
public:
  struct State : public candidate_window::View {
    State() : candidates_(std::vector<std::wstring>()) {}

    // candidate_window::View
    virtual const std::vector<std::wstring> *candidates() const override {
      return &candidates_;
    }

    virtual int current_index() const override { return current_index_; }

    std::vector<std::wstring> candidates_;

    int current_index_ = 0;
  };
  ~CandidateList();

  //  CandidateListUI::Handlers
  virtual HRESULT OnLayoutChange(ITfContext *, ITfContextView *);

  void Update(const senn::win::im::views::Editing &);
  void Update(const senn::win::im::views::Converting &);

  static CandidateList *Create(ITfContext *, ITfThreadMgr *, ITfComposition *,
                               TfClientId);

private:
  State state_;

  CandidateListUI *ui_;

  ITfComposition *composition_;

  TfClientId client_id_;

  CandidateList(ITfComposition *, TfClientId);
};

typedef Holder<CandidateList> CandidateListHolder;

class EditSessionImplementingIUnknown : public ITfEditSession {
public:
  // IUnknow
  HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject) {
    if (ppvObject == NULL) {
      return E_INVALIDARG;
    }

    if (IsEqualIID(riid, IID_IUnknown) ||
        IsEqualIID(riid, IID_ITfEditSession)) {
      *ppvObject = (ITfLangBarItem *)this;
    } else {
      *ppvObject = NULL;
      return E_NOINTERFACE;
    }

    AddRef();
    return S_OK;
  }

  ULONG __stdcall AddRef(void) { return ++ref_count_; }

  ULONG __stdcall Release(void) {
    if (ref_count_ <= 0) {
      return 0;
    }

    const ULONG count = --ref_count_;
    if (count == 0) {
      delete this;
    }
    return count;
  }

  virtual HRESULT __stdcall DoEditSession(TfEditCookie ec) = 0;

  virtual ~EditSessionImplementingIUnknown() {}

private:
  ULONG ref_count_ = 1;
};

class EditSessionEditing : public EditSessionImplementingIUnknown {
public:
  EditSessionEditing(const senn::win::im::views::Editing &, ITfContext *,
                     ITfThreadMgr *, TfClientId, TfGuidAtom,
                     ITfCompositionSink *, CompositionHolder *,
                     CandidateListHolder *);
  ~EditSessionEditing() override;

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  const senn::win::im::views::Editing view_;

  ITfContext *const context_;

  ITfThreadMgr *const thread_mgr_;

  TfClientId client_id_;

  const TfGuidAtom display_attribute_atom_;

  ITfCompositionSink *composition_sink_;

  CompositionHolder *const composition_holder_;

  CandidateListHolder *const candidate_list_holder_;
};

class EditSessionConverting : public EditSessionImplementingIUnknown {
public:
  struct DisplayAttributeAtoms {
    TfGuidAtom non_focused, focused;
  };

  EditSessionConverting(const senn::win::im::views::Converting &, ITfContext *,
                        ITfThreadMgr *, TfClientId,
                        const DisplayAttributeAtoms *, ITfComposition *,
                        CandidateListHolder *);
  ~EditSessionConverting() override;

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  const senn::win::im::views::Converting view_;

  ITfContext *const context_;

  ITfThreadMgr *const thread_mgr_;

  TfClientId client_id_;

  const DisplayAttributeAtoms *atoms_;

  ITfComposition *const composition_;

  CandidateListHolder *const candidate_list_holder_;
};

class EditSessionCommitted : public EditSessionImplementingIUnknown {
public:
  EditSessionCommitted(const senn::win::im::views::Committed &, ITfContext *,
                       ITfCompositionSink *, CompositionHolder *,
                       CandidateListHolder *);
  ~EditSessionCommitted() override;

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  const senn::win::im::views::Committed view_;

  ITfContext *const context_;

  ITfCompositionSink *composition_sink_;

  CompositionHolder *const composition_holder_;

  CandidateListHolder *const candidate_list_holder_;
};

class MoveCandidateWindowToTextPositionEditSession
    : public EditSessionImplementingIUnknown {
public:
  MoveCandidateWindowToTextPositionEditSession(ITfContextView *context_view,
                                               ITfComposition *composition,
                                               CandidateListUI *ui)
      : context_view_(context_view), composition_(composition), ui_(ui) {
    composition_->AddRef();
    context_view_->AddRef();
  }

  ~MoveCandidateWindowToTextPositionEditSession() override {
    composition_->Release();
    context_view_->Release();
  }

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  ITfContextView *const context_view_;

  ITfComposition *const composition_;

  CandidateListUI *ui_;
};

class KeyEventHandler {
public:
  class Handlers {
  public:
    virtual void OnToggleInputModeKeyDown() = 0;
  };

  KeyEventHandler(ITfThreadMgr *, TfClientId, ITfCompositionSink *,
                  senn::win::im::StatefulIME *, TfGuidAtom,
                  EditSessionConverting::DisplayAttributeAtoms *, Handlers *);

  ~KeyEventHandler();

  HRESULT OnSetFocus(BOOL fForeground);
  HRESULT OnTestKeyDown(ITfContext *pic, WPARAM wParam, LPARAM lParam,
                        BOOL *pfEaten);
  HRESULT OnTestKeyUp(ITfContext *pic, WPARAM wParam, LPARAM lParam,
                      BOOL *pfEaten);
  HRESULT OnKeyDown(ITfContext *pic, WPARAM wParam, LPARAM lParam,
                    BOOL *pfEaten);
  HRESULT OnKeyUp(ITfContext *pic, WPARAM wParam, LPARAM lParam, BOOL *pfEaten);
  HRESULT OnPreservedKey(ITfContext *pic, REFGUID rguid, BOOL *pfEaten);

private:
  ITfThreadMgr *thread_mgr_;

  TfClientId client_id_;

  ITfCompositionSink *composition_sink_;

  // The input method that manages the states.
  senn::win::im::StatefulIME *ime_;

  // Value of the style for decorating a text when editing
  TfGuidAtom editing_display_attribute_atom_;

  // Values of the style for decorating a text when converting
  const EditSessionConverting::DisplayAttributeAtoms
      *converting_display_attribute_atoms_;

  Handlers *handlers_;

  CompositionHolder composition_holder_;

  CandidateListHolder candidate_list_holder_;
};

} // namespace text_service
} // namespace senn_win
} // namespace senn
