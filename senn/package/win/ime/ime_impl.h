#pragma once
#include "../../../src-cpp/win/im/stateful_ime.h"
#include "ime_h.h"

extern LONG g_locks;

typedef senn::win::im::StatefulIME *(*MakeStatefulIMEFn)();

class SennIME : public ISennIME {
public:
  SennIME(senn::win::im::StatefulIME *ime) : count_(1), ime_(ime) {}

  // ISennIME ÇâÓÇµÇƒåpè≥Ç≥ÇÍÇ‹ÇµÇΩ
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }

    if (riid == IID_IUnknown || riid == IID_ISennIME) {
      *ppvObject = static_cast<ISennIME *>(this);
    } else {
      return E_NOINTERFACE;
    }
    AddRef();

    return S_OK;
  }
  virtual ULONG __stdcall AddRef(void) override {
    return InterlockedIncrement(&count_);
  }
  virtual ULONG __stdcall Release(void) override {
    ULONG count = InterlockedDecrement(&count_);
    if (count == 0) {
      delete this;
    }
    return count;
  }
  virtual HRESULT __stdcall CanProcess(ULONGLONG keycode,
                                       BOOL *result) override {
    if (result == nullptr) {
      return E_INVALIDARG;
    }
    *result = ime_->CanProcess(keycode);
    return S_OK;
  }
  virtual HRESULT __stdcall ProcessInput(ULONGLONG keycode, BYTE *modifiers,
                                         SennView **view) override {
    if (view == nullptr) {
      return E_INVALIDARG;
    }
    return S_OK;
  }
  virtual HRESULT __stdcall ToggleInputMode(void) override { return E_NOTIMPL; }

  virtual HRESULT __stdcall GetInputMode(SennInputMode *mode) override {
    if (mode == nullptr) {
      return E_INVALIDARG;
    }
    switch (ime_->GetInputMode()) {
    case senn::win::im::InputMode::kDirect:
      *mode = SennInputMode::kDirect;
      break;
    case senn::win::im::InputMode::kHiragana:
      *mode = SennInputMode::kHiragana;
      break;
    default:
      *mode = SennInputMode::kUnknown;
    }
    return S_OK;
  }

private:
  LONG count_;

  senn::win::im::StatefulIME *ime_;
};

class IMEClassFactory : public IClassFactory {
public:
  IMEClassFactory(MakeStatefulIMEFn make_stateful_ime_fn)
      : count_(1), make_stateful_ime_fn_(make_stateful_ime_fn) {}

  // IClassFactory ÇâÓÇµÇƒåpè≥Ç≥ÇÍÇ‹ÇµÇΩ
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }

    if (riid == IID_IUnknown || riid == IID_IClassFactory) {
      *ppvObject = static_cast<IClassFactory *>(this);
    } else {
      return E_NOINTERFACE;
    }
    AddRef();

    return S_OK;
  }
  virtual ULONG __stdcall AddRef(void) override {
    LockModule();
    return InterlockedIncrement(&count_);
  }

  virtual ULONG __stdcall Release(void) override {
    ULONG count = InterlockedDecrement(&count_);
    if (count == 0) {
      UnlockModule();
      delete this;
    }
    return count;
  }

  virtual HRESULT __stdcall CreateInstance(IUnknown *pUnkOuter, REFIID riid,
                                           void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }
    if (pUnkOuter != nullptr) {
      return CLASS_E_NOAGGREGATION;
    }

    *ppvObject = nullptr;

    SennIME *sennIME = new SennIME(make_stateful_ime_fn_());
    if (sennIME == nullptr) {
      return E_OUTOFMEMORY;
    }

    HRESULT result = sennIME->QueryInterface(riid, ppvObject);
    sennIME->Release();

    return result;
  }
  virtual HRESULT __stdcall LockServer(BOOL fLock) override {
    if (fLock) {
      LockModule();
    } else {
      UnlockModule();
    }
    return S_OK;
  }

private:
  LONG count_;

  MakeStatefulIMEFn make_stateful_ime_fn_;

  void LockModule() { InterlockedIncrement(&g_locks); }

  void UnlockModule() { InterlockedDecrement(&g_locks); }
};
