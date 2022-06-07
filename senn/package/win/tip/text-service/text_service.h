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
  virtual HRESULT __stdcall QueryInterface(REFIID riid,
                                           void **ppvObject) override;
  virtual ULONG __stdcall AddRef(void) override;
  virtual ULONG __stdcall Release(void) override;

  // ITfKeyEventSink
  virtual HRESULT __stdcall OnSetFocus(BOOL fForeground) override;
  virtual HRESULT __stdcall OnTestKeyDown(ITfContext *pic, WPARAM wParam,
                                          LPARAM lParam,
                                          BOOL *pfEaten) override;
  virtual HRESULT __stdcall OnTestKeyUp(ITfContext *pic, WPARAM wParam,
                                        LPARAM lParam, BOOL *pfEaten) override;
  virtual HRESULT __stdcall OnKeyDown(ITfContext *pic, WPARAM wParam,
                                      LPARAM lParam, BOOL *pfEaten) override;
  virtual HRESULT __stdcall OnKeyUp(ITfContext *pic, WPARAM wParam,
                                    LPARAM lParam, BOOL *pfEaten) override;
  virtual HRESULT __stdcall OnPreservedKey(ITfContext *pic, REFGUID rguid,
                                           BOOL *pfEaten) override;

  // ITfDisplayAttributeProvider
  virtual HRESULT __stdcall EnumDisplayAttributeInfo(
      IEnumTfDisplayAttributeInfo **ppEnum) override;
  virtual HRESULT __stdcall GetDisplayAttributeInfo(
      REFGUID guid, ITfDisplayAttributeInfo **ppInfo) override;

  // ITfCompositionSink
  virtual HRESULT __stdcall OnCompositionTerminated(
      TfEditCookie ecWrite, ITfComposition *pComposition) override;

  // ITfTextInputProcessor
  virtual HRESULT __stdcall Activate(ITfThreadMgr *ptim,
                                     TfClientId tid) override;
  virtual HRESULT __stdcall Deactivate(void) override;

  // langbar::InputModeToggleButton::View
  virtual void GetIcon(HICON *) const override;

  // langbar::InputModeToggleButton::Handlers
  virtual void OnClickInputModelToggleButton() override;

  // KeyEventHandler::Handlers
  virtual void OnToggleInputModeKeyDown() override;

  void ToggleInputMode();

  HRESULT ActivateInternal(ITfThreadMgr *thread_mgr, TfClientId client_id);

private:
  CLSID clsid_text_service_;

  ITfThreadMgr *thread_mgr_;

  TfClientId client_id_;

  // Button to switch the current input mode.
  langbar::InputModeToggleButton *input_mode_toggle_button_;

  senn::win::im::StatefulIME *ime_;

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
