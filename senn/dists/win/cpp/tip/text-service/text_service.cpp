#include "text_service.h"
#include "../ime/stateful_im_proxy_ipc.h"
#include "hiragana/ui.h"
#include "object_releaser.h"

namespace senn {
namespace senn_win {
namespace text_service {

// ITfTextInputProcessor

HRESULT TextService::Activate(ITfThreadMgr *thread_mgr, TfClientId client_id) {
  thread_mgr_ = thread_mgr;
  thread_mgr->AddRef();

  client_id_ = client_id;

  HRESULT result;
  // Add language bar items.
  {
    ITfLangBarItemMgr *lang_bar_item_mgr;
    if (thread_mgr->QueryInterface(IID_ITfLangBarItemMgr,
                                   (void **)&lang_bar_item_mgr) != S_OK) {
      return E_FAIL;
    }
    input_mode_toggle_button_ = new langbar::InputModeToggleButton(
        clsid_text_service_, 0,
        static_cast<langbar::InputModeToggleButton::State *>(this),
        static_cast<langbar::InputModeToggleButton::Handlers *>(this));
    lang_bar_item_mgr->AddItem(input_mode_toggle_button_);

    lang_bar_item_mgr->Release();
  }

  // Advice key event sink to receive key input notifications.
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
        {hiragana::ui::editing::kDisplayAttributeGuid,
         &editing_display_attribute_atom_},
        {hiragana::ui::converting::non_focused::kDisplayAttributeGuid,
         &converting_display_attribute_atoms_.non_focused},
        {hiragana::ui::converting::focused::kDisplayAttributeGuid,
         &converting_display_attribute_atoms_.focused}};

    for (size_t i = 0; i < ARRAYSIZE(settings); i++) {
      result = category_mgr->RegisterGUID(settings[i].guid, settings[i].output);
      if (FAILED(result)) {
        return result;
      }
    }
  }

  // Create a stateful IM to process user inputs of keys.
  {
    senn::senn_win::ime::StatefulIM *im =
        senn::senn_win::ime::StatefulIMProxyIPC::Create(kNamedPipePath);
    if (im == nullptr) {
      return E_FAIL;
    }
    hiragana_key_event_handler_ = new hiragana::HiraganaKeyEventHandler(
        thread_mgr_, client_id, static_cast<ITfCompositionSink *>(this), im,
        editing_display_attribute_atom_, &converting_display_attribute_atoms_);
  }

  direct_key_event_handler_ = new direct::DirectKeyEventHandler();

  return result;
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
  *attribute_info = new hiragana::ui::EnumDisplayAttributeInfo();
  return S_OK;
}

HRESULT __stdcall TextService::GetDisplayAttributeInfo(
    REFGUID guid, ITfDisplayAttributeInfo **attribute) {
  if (attribute == nullptr) {
    return E_INVALIDARG;
  }
  if (IsEqualGUID(guid, hiragana::ui::editing::kDisplayAttributeGuid)) {
    *attribute = new hiragana::ui::editing::DisplayAttributeInfo();
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
  switch (input_mode_) {
  case senn::senn_win::text_service::kDirect:
    return direct_key_event_handler_->OnSetFocus(fForeground);
  case senn::senn_win::text_service::kHiragana:
    return hiragana_key_event_handler_->OnSetFocus(fForeground);
  default:
    break;
  }
  return E_FAIL;
}

HRESULT __stdcall TextService::OnTestKeyDown(ITfContext *context, WPARAM wParam,
                                             LPARAM lParam, BOOL *pfEaten) {
  if (wParam == 0xF3 || wParam == 0xF4) {
    // hankaku/zenkaku key
    ToggleInputMode();
    *pfEaten = false;
    return S_OK;
  }

  switch (input_mode_) {
  case senn::senn_win::text_service::kDirect:
    return direct_key_event_handler_->OnTestKeyDown(context, wParam, lParam,
                                                    pfEaten);
  case senn::senn_win::text_service::kHiragana:
    return hiragana_key_event_handler_->OnTestKeyDown(context, wParam, lParam,
                                                      pfEaten);
  default:
    break;
  }
  return E_FAIL;
}

HRESULT __stdcall TextService::OnKeyDown(ITfContext *context, WPARAM wParam,
                                         LPARAM lParam, BOOL *pfEaten) {
  switch (input_mode_) {
  case senn::senn_win::text_service::kDirect:
    return direct_key_event_handler_->OnKeyDown(context, wParam, lParam,
                                                pfEaten);
  case senn::senn_win::text_service::kHiragana:
    return hiragana_key_event_handler_->OnKeyDown(context, wParam, lParam,
                                                  pfEaten);
  default:
    break;
  }
  return E_FAIL;
}

HRESULT __stdcall TextService::OnTestKeyUp(ITfContext *context, WPARAM wParam,
                                           LPARAM lParam, BOOL *pfEaten) {
  switch (input_mode_) {
  case senn::senn_win::text_service::kDirect:
    return direct_key_event_handler_->OnTestKeyUp(context, wParam, lParam,
                                                  pfEaten);
  case senn::senn_win::text_service::kHiragana:
    return hiragana_key_event_handler_->OnTestKeyUp(context, wParam, lParam,
                                                    pfEaten);
  default:
    break;
  }
  return E_FAIL;
}

HRESULT __stdcall TextService::OnKeyUp(ITfContext *context, WPARAM wParam,
                                       LPARAM lParam, BOOL *pfEaten) {
  switch (input_mode_) {
  case senn::senn_win::text_service::kDirect:
    return direct_key_event_handler_->OnKeyUp(context, wParam, lParam, pfEaten);
  case senn::senn_win::text_service::kHiragana:
    return hiragana_key_event_handler_->OnKeyUp(context, wParam, lParam,
                                                pfEaten);
  default:
    break;
  }
  return E_FAIL;
}

HRESULT __stdcall TextService::OnPreservedKey(ITfContext *context,
                                              REFGUID rguid, BOOL *pfEaten) {
  switch (input_mode_) {
  case senn::senn_win::text_service::kDirect:
    return direct_key_event_handler_->OnPreservedKey(context, rguid, pfEaten);
  case senn::senn_win::text_service::kHiragana:
    return hiragana_key_event_handler_->OnPreservedKey(context, rguid, pfEaten);
  default:
    break;
  }
  return E_FAIL;
}

// langbar::InputModeToggleButton::State

InputMode TextService::input_mode() const { return input_mode_; }

// langbar::InputModeToggleButton::Handlers

void TextService::ToggleInputMode() {
  if (input_mode_ == InputMode::kDirect) {
    input_mode_ = InputMode::kHiragana;
  } else {
    input_mode_ = InputMode::kDirect;
  }

  input_mode_toggle_button_->item_sink()->OnUpdate(TF_LBI_ICON);
}

} // namespace text_service
} // namespace senn_win
} // namespace senn
