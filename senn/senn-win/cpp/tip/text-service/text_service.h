#pragma once

#include <msctf.h>
#include <windows.h>

#include <string>

#include "../senn.h"
#include "../win/text-service/class_factory.h"
#include "../ime/stateful_im_proxy.h"

namespace senn {
namespace senn_win {
namespace text_service {

class EditSession : public ITfEditSession {
public:
  EditSession(ITfCompositionSink*,
              ITfContext*,
              ITfComposition**,
              const std::wstring&,
              TfGuidAtom);
  ~EditSession();

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

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec);

  ITfCompositionSink *composition_sink_;
  
  ITfContext* const context_;

  const std::wstring text_;

  ITfComposition** const composition_;

  const TfGuidAtom editing_atom_;

  ULONG ref_count_ = 1;
};


class TextService
    : public ITfKeyEventSink,
      public ITfDisplayAttributeProvider,
      public ITfCompositionSink,
      public ITfTextInputProcessor {
public:
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
  ::senn::senn_win::ime::StatefulIMProxy *stateful_im_;


  ITfThreadMgr *thread_mgr_;

  TfClientId client_id_;


  ITfContext *context_ = nullptr;

  ITfComposition *composition_;

  TfGuidAtom editing_atom_;


  struct {
    TfGuidAtom editing_atom = TF_INVALID_GUIDATOM;
  } display_attribute_vals_;


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
