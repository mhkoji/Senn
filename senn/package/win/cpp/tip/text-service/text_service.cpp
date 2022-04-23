#include "text_service.h"
#include "object_releaser.h"
#include "ui.h"
#include <cassert>
#include <win/im/stateful_ime_proxy.h>

namespace senn {
namespace senn_win {
namespace text_service {

// ITfTextInputProcessor

HRESULT TextService::Activate(ITfThreadMgr *thread_mgr, TfClientId client_id) {
  HRESULT result = ActivateInternal(thread_mgr, client_id);
  if (FAILED(result)) {
    Deactivate();
  }
  return result;
}

HRESULT TextService::ActivateInternal(ITfThreadMgr *thread_mgr,
                                      TfClientId client_id) {
  thread_mgr_ = thread_mgr;
  thread_mgr->AddRef();

  client_id_ = client_id;

  HRESULT result;

  // Register guids for display attribute to decorate composing texts.
  {
    ITfCategoryMgr *category_mgr;
    result =
        CoCreateInstance(CLSID_TF_CategoryMgr, nullptr, CLSCTX_INPROC_SERVER,
                         IID_ITfCategoryMgr, (void **)&category_mgr);
    if (FAILED(result)) {
      return result;
    }
    ObjectReleaser<ITfCategoryMgr> category_mgr_releaser(category_mgr);

    struct {
      const GUID &guid;
      TfGuidAtom *output;
    } settings[] = {
        {ui::editing::kDisplayAttributeGuid, &editing_display_attribute_atom_},
        {ui::converting::non_focused::kDisplayAttributeGuid,
         &converting_display_attribute_atoms_.non_focused},
        {ui::converting::focused::kDisplayAttributeGuid,
         &converting_display_attribute_atoms_.focused}};

    for (size_t i = 0; i < ARRAYSIZE(settings); i++) {
      result = category_mgr->RegisterGUID(settings[i].guid, settings[i].output);
      if (FAILED(result)) {
        return result;
      }
    }
  }

  // Create a key event handle that processes user keyboard inputs
  {
    // Load dll for TCP
    WSADATA wsaData;
    if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
      return E_FAIL;
    }

    // Create a stateful IM to process user inputs of keys.
    ime_ = senn::senn_win::ime::StatefulIMEProxy::CreateTCPPRoxy("localhost",
                                                                 "5678");
    // senn::senn_win::ime::StatefulIMEProxy::CreateIPCPRoxy(kNamedPipePath);
    if (ime_ == nullptr) {
      return E_FAIL;
    }

    key_event_handler_ = new KeyEventHandler(
        thread_mgr_, client_id_, static_cast<ITfCompositionSink *>(this), ime_,
        editing_display_attribute_atom_, &converting_display_attribute_atoms_,
        static_cast<KeyEventHandler::Handlers *>(this));
  }

  // Advice key event sink to receive key input notifications.
  // Execute this after initialing key_event_handler_ because
  // after calling AdviseKeyEventSink key events are distpached before finishing
  // Activate
  {
    ITfKeystrokeMgr *keystroke_mgr;
    if (thread_mgr->QueryInterface(IID_ITfKeystrokeMgr,
                                   (void **)&keystroke_mgr) != S_OK) {
      return E_FAIL;
    }

    result = keystroke_mgr->AdviseKeyEventSink(
        client_id, static_cast<ITfKeyEventSink *>(this), TRUE);

    keystroke_mgr->Release();

    if (FAILED(result)) {
      return result;
    }
  }

  // Add language bar items.
  {
    ITfLangBarItemMgr *lang_bar_item_mgr;
    if (thread_mgr->QueryInterface(IID_ITfLangBarItemMgr,
                                   (void **)&lang_bar_item_mgr) != S_OK) {
      return E_FAIL;
    }
    input_mode_toggle_button_ = new langbar::InputModeToggleButton(
        clsid_text_service_, 0,
        static_cast<langbar::InputModeToggleButton::View *>(this),
        static_cast<langbar::InputModeToggleButton::Handlers *>(this));
    lang_bar_item_mgr->AddItem(input_mode_toggle_button_);

    lang_bar_item_mgr->Release();
  }

  return S_OK;
}

HRESULT TextService::Deactivate() {
  if (thread_mgr_ == nullptr) {
    return S_OK;
  }

  // Remove language bar items.
  {
    ITfLangBarItemMgr *lang_bar_item_mgr;
    if (thread_mgr_->QueryInterface(IID_ITfLangBarItemMgr,
                                    (void **)&lang_bar_item_mgr) != S_OK) {
      return S_OK;
    }
    if (input_mode_toggle_button_ != nullptr) {
      lang_bar_item_mgr->RemoveItem(input_mode_toggle_button_);
      input_mode_toggle_button_->Release();
      input_mode_toggle_button_ = nullptr;
    }

    lang_bar_item_mgr->Release();
  }

  {
    ITfKeystrokeMgr *keystroke_mgr;
    if (thread_mgr_->QueryInterface(IID_ITfKeystrokeMgr,
                                    (void **)&keystroke_mgr) != S_OK) {
      return S_OK;
    }
    keystroke_mgr->UnadviseKeyEventSink(client_id_);
    keystroke_mgr->Release();
  }

  if (key_event_handler_ != nullptr) {
    delete key_event_handler_;
  }

  if (ime_ != nullptr) {
    delete ime_;
  }

  // Unload dll for TCP
  WSACleanup();

  thread_mgr_->Release();
  thread_mgr_ = nullptr;
  return S_OK;
}

// ITfDisplayAttributeProvider

HRESULT __stdcall TextService::EnumDisplayAttributeInfo(
    IEnumTfDisplayAttributeInfo **attribute_info) {
  if (attribute_info == nullptr) {
    return E_INVALIDARG;
  }
  *attribute_info = new ui::EnumDisplayAttributeInfo();
  return S_OK;
}

HRESULT __stdcall TextService::GetDisplayAttributeInfo(
    REFGUID guid, ITfDisplayAttributeInfo **attribute) {
  if (attribute == nullptr) {
    return E_INVALIDARG;
  }
  if (IsEqualGUID(guid, ui::editing::kDisplayAttributeGuid)) {
    *attribute = new ui::editing::DisplayAttributeInfo();
  } else {
    *attribute = nullptr;
    return E_INVALIDARG;
  }
  return S_OK;
}

HRESULT __stdcall TextService::OnCompositionTerminated(
    TfEditCookie ecWrite, ITfComposition *pComposition) {
  return S_OK;
}

// ITfKeyEventSink

HRESULT __stdcall TextService::OnSetFocus(BOOL fForeground) {
  assert(key_event_handler_ != nullptr);
  return key_event_handler_->OnSetFocus(fForeground);
}

HRESULT __stdcall TextService::OnTestKeyDown(ITfContext *context, WPARAM wParam,
                                             LPARAM lParam, BOOL *pfEaten) {
  assert(key_event_handler_ != nullptr);
  return key_event_handler_->OnTestKeyDown(context, wParam, lParam, pfEaten);
}

HRESULT __stdcall TextService::OnKeyDown(ITfContext *context, WPARAM wParam,
                                         LPARAM lParam, BOOL *pfEaten) {
  assert(key_event_handler_ != nullptr);
  return key_event_handler_->OnKeyDown(context, wParam, lParam, pfEaten);
}

HRESULT __stdcall TextService::OnTestKeyUp(ITfContext *context, WPARAM wParam,
                                           LPARAM lParam, BOOL *pfEaten) {
  assert(key_event_handler_ != nullptr);
  return key_event_handler_->OnTestKeyUp(context, wParam, lParam, pfEaten);
}

HRESULT __stdcall TextService::OnKeyUp(ITfContext *context, WPARAM wParam,
                                       LPARAM lParam, BOOL *pfEaten) {
  assert(key_event_handler_ != nullptr);
  return key_event_handler_->OnKeyUp(context, wParam, lParam, pfEaten);
}

HRESULT __stdcall TextService::OnPreservedKey(ITfContext *context,
                                              REFGUID rguid, BOOL *pfEaten) {
  assert(key_event_handler_ != nullptr);
  return key_event_handler_->OnPreservedKey(context, rguid, pfEaten);
}

// langbar::InputModeToggleButton::State
void TextService::GetIcon(HICON *phIcon) const {
  assert(ime_ != nullptr);
  senn::senn_win::ime::InputMode mode = ime_->GetInputMode();

  // Use a built-in icon for a while...
  if (mode == senn::senn_win::ime::InputMode::kDirect) {
    *phIcon = LoadIcon(NULL, IDI_APPLICATION);
  } else {
    *phIcon = LoadIcon(NULL, IDI_ASTERISK);
  }
}

// langbar::InputModeToggleButton::Handlers
void TextService::OnClickInputModelToggleButton() { ToggleInputMode(); }

// KeyEventHandler::Handlers
void TextService::OnToggleInputModeKeyDown() { ToggleInputMode(); }

void TextService::ToggleInputMode() {
  assert(ime_ != nullptr);
  // Update state
  ime_->ToggleInputMode();
  // Update ui (UI calls GetIcon to get the current state of input mode.)
  input_mode_toggle_button_->item_sink()->OnUpdate(TF_LBI_ICON);
}

} // namespace text_service
} // namespace senn_win
} // namespace senn
