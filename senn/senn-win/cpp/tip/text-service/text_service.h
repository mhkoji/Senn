#pragma once

#include <msctf.h>
#include <windows.h>

#include <string>

#include "../senn.h"
#include "../win/text-service/class_factory.h"
#include "../ime/stateful_im.h"

namespace senn {
namespace senn_win {
namespace text_service {

class CompositionHolder {
public:

  CompositionHolder() : composition_(nullptr) {}

  ITfComposition *Get() {
    return composition_;
  }

  void Set(ITfComposition *c) {
    composition_ = c;
  }

private:
  ITfComposition *composition_;
};


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
    }
    else {
      *ppvObject = NULL;
      return E_NOINTERFACE;
    }
    AddRef();
    return S_OK;
  }

  ULONG __stdcall AddRef(void) {
    return ++ref_count_;
  }

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
  EditSessionEditing(
      const senn::senn_win::ime::views::Editing&,
      ITfContext*,
      TfGuidAtom,
      ITfCompositionSink*,
      CompositionHolder*);
  ~EditSessionEditing() override;
 
private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  const senn::senn_win::ime::views::Editing view_;

  ITfContext* const context_;

  const TfGuidAtom display_attribute_atom_;

  ITfCompositionSink *composition_sink_;

  CompositionHolder* const composition_holder_;
};

class EditSessionConverting : public EditSessionImplementingIUnknown {
public:
  struct DisplayAttributeAtoms {
    TfGuidAtom non_focused, focused;
  };

  EditSessionConverting(
    const senn::senn_win::ime::views::Converting&,
    ITfContext*,
    const DisplayAttributeAtoms&,
    ITfComposition*);
  ~EditSessionConverting() override;

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  const senn::senn_win::ime::views::Converting view_;

  ITfContext* const context_;

  const DisplayAttributeAtoms atoms_;

  ITfComposition* const composition_;
};

class EditSessionCommitted : public EditSessionImplementingIUnknown {
public:
  EditSessionCommitted(
      const senn::senn_win::ime::views::Committed&,
      ITfContext*,
      ITfCompositionSink*,
      CompositionHolder*);
  ~EditSessionCommitted() override;

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  const senn::senn_win::ime::views::Committed view_;

  ITfContext* const context_;

  ITfCompositionSink *composition_sink_;

  CompositionHolder* const composition_holder_;
};


class TextService
    : public ITfKeyEventSink,
      public ITfDisplayAttributeProvider,
      public ITfCompositionSink,
      public ITfTextInputProcessor {
public:

  TextService()
    : stateful_im_(nullptr),
      clsid_text_service_(kClsid),
      thread_mgr_(nullptr),
      client_id_(TF_CLIENTID_NULL),
      input_mode_toggle_button_(nullptr),
      composition_holder_(CompositionHolder()),
      editing_display_attribute_atom_(TF_INVALID_GUIDATOM) {}

  // IUnknow
  HRESULT __stdcall QueryInterface(REFIID riid, void** ppvObject) {
    if (ppvObject == NULL) {
      return E_INVALIDARG;
    }
    if (IsEqualIID(riid, IID_IUnknown) ||
        IsEqualIID(riid, IID_ITfTextInputProcessor)) {
      *ppvObject = static_cast<ITfTextInputProcessor *>(this);
    } else if (IsEqualIID(riid, IID_ITfKeyEventSink)) {
      *ppvObject = static_cast<ITfKeyEventSink *>(this);
    } else if (IsEqualIID(riid, IID_ITfDisplayAttributeProvider)) {
      *ppvObject = static_cast<ITfDisplayAttributeProvider *>(this);
    } else if (IsEqualIID(riid, IID_ITfDisplayAttributeProvider)) {
      *ppvObject = static_cast<ITfCompositionSink *>(this);
    } else {
      *ppvObject = NULL;
      return E_NOINTERFACE;
    }
    AddRef();
    return S_OK;
  }

  ULONG __stdcall AddRef(void) {
    return ++ref_count_;
  }

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

  // ITfKeyEventSink
  HRESULT __stdcall OnSetFocus(BOOL fForeground) override;
  HRESULT __stdcall OnTestKeyDown(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnTestKeyUp(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnKeyDown(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnKeyUp(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnPreservedKey(ITfContext*, REFGUID, BOOL*) override;

  // ITfTextInputProcessor
  HRESULT Activate(ITfThreadMgr*, TfClientId);
  HRESULT Deactivate();

  // ITfCompositionSink
  HRESULT __stdcall OnCompositionTerminated(TfEditCookie, ITfComposition*) override;

  // ITfDisplayAttributeProvider
  HRESULT __stdcall EnumDisplayAttributeInfo(IEnumTfDisplayAttributeInfo**) override;
  HRESULT __stdcall GetDisplayAttributeInfo(REFGUID, ITfDisplayAttributeInfo**) override;

private:
  // The input method that manages the states.
  ::senn::senn_win::ime::StatefulIM *stateful_im_;

  CLSID clsid_text_service_;

  ITfThreadMgr *thread_mgr_;

  TfClientId client_id_;

  ITfLangBarItem *input_mode_toggle_button_;

  CompositionHolder composition_holder_;

  // Value of the style for decorating a text when editing
  TfGuidAtom editing_display_attribute_atom_;

  // Values of the style for decorating a text when converting
  EditSessionConverting::DisplayAttributeAtoms converting_display_attribute_atoms_;


  ULONG ref_count_ = 1;
};

class TextServiceFactory
    : public senn::win::text_service::ClassFactory<TextService> {
public:
  class ServerLocker {
  public:
    virtual ~ServerLocker() {};

    virtual void Lock() = 0;

    virtual void Unlock() = 0;
  };

  TextServiceFactory(ServerLocker* const locker) : locker_(locker) {}

  REFIID GetIid() {
    return kClsid;
  }
 
  HRESULT __stdcall LockServer(BOOL lock) override {
    if (lock) {
      locker_->Lock();
    } else {
      locker_->Unlock();
    }
    return S_OK;
  }

private:

  ServerLocker* const locker_;
};

} // text_service
} // win
} // senn
