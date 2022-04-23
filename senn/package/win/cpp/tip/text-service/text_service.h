#pragma once

#include <msctf.h>
#include <string>
#include <windows.h>

#include "../senn.h"
#include "../win/text-service/class_factory.h"
#include "direct.h"
#include "key_event_handler.h"
#include "langbar.h"
#include "ui.h"
#include <win/im/stateful_ime.h>

namespace senn {
namespace senn_win {
namespace text_service {

class TextService : public ITfKeyEventSink,
                    public ITfDisplayAttributeProvider,
                    public ITfCompositionSink,
                    public ITfTextInputProcessor,
                    public langbar::InputModeToggleButton::View,
                    public langbar::InputModeToggleButton::Handlers,
                    public KeyEventHandler::Handlers {
public:
  TextService()
      : clsid_text_service_(kClsid), thread_mgr_(nullptr),
        client_id_(TF_CLIENTID_NULL), input_mode_toggle_button_(nullptr),
        ime_(nullptr), key_event_handler_(nullptr),
        editing_display_attribute_atom_(TF_INVALID_GUIDATOM),
        converting_display_attribute_atoms_(
            {TF_INVALID_GUIDATOM, TF_INVALID_GUIDATOM}),
        ref_count_(1) {}

  // IUnknow
  HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject) {
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

  // ITfKeyEventSink
  HRESULT __stdcall OnSetFocus(BOOL fForeground) override;
  HRESULT __stdcall OnTestKeyDown(ITfContext *, WPARAM, LPARAM,
                                  BOOL *) override;
  HRESULT __stdcall OnTestKeyUp(ITfContext *, WPARAM, LPARAM, BOOL *) override;
  HRESULT __stdcall OnKeyDown(ITfContext *, WPARAM, LPARAM, BOOL *) override;
  HRESULT __stdcall OnKeyUp(ITfContext *, WPARAM, LPARAM, BOOL *) override;
  HRESULT __stdcall OnPreservedKey(ITfContext *, REFGUID, BOOL *) override;

  // ITfTextInputProcessor
  HRESULT Activate(ITfThreadMgr *, TfClientId);
  HRESULT Deactivate();

  // ITfCompositionSink
  HRESULT __stdcall OnCompositionTerminated(TfEditCookie,
                                            ITfComposition *) override;

  // ITfDisplayAttributeProvider
  HRESULT __stdcall EnumDisplayAttributeInfo(
      IEnumTfDisplayAttributeInfo **) override;
  HRESULT __stdcall GetDisplayAttributeInfo(
      REFGUID, ITfDisplayAttributeInfo **) override;

  HRESULT ActivateInternal(ITfThreadMgr *thread_mgr, TfClientId client_id);

  // langbar::InputModeToggleButton::View
  virtual void GetIcon(HICON *) const override;

  // langbar::InputModeToggleButton::Handlers
  virtual void OnClickInputModelToggleButton() override;

  // KeyEventHandler::Handlers
  virtual void OnToggleInputModeKeyDown() override;

  void ToggleInputMode();

private:
  CLSID clsid_text_service_;

  ITfThreadMgr *thread_mgr_;

  TfClientId client_id_;

  // Button to switch the current input mode.
  langbar::InputModeToggleButton *input_mode_toggle_button_;

  senn::senn_win::ime::StatefulIME *ime_;

  KeyEventHandler *key_event_handler_;

  // Value of the style for decorating a text when editing
  TfGuidAtom editing_display_attribute_atom_;

  // Values of the style for decorating a text when converting
  EditSessionConverting::DisplayAttributeAtoms
      converting_display_attribute_atoms_;

  ULONG ref_count_;
};

class TextServiceFactory
    : public senn::win::text_service::ClassFactory<TextService> {
public:
  class ServerLocker {
  public:
    virtual ~ServerLocker(){};

    virtual void Lock() = 0;

    virtual void Unlock() = 0;
  };

  TextServiceFactory(ServerLocker *const locker) : locker_(locker) {}

  REFIID GetIid() { return kClsid; }

  HRESULT __stdcall LockServer(BOOL lock) override {
    if (lock) {
      locker_->Lock();
    } else {
      locker_->Unlock();
    }
    return S_OK;
  }

private:
  ServerLocker *const locker_;
};

} // namespace text_service
} // namespace senn_win
} // namespace senn
