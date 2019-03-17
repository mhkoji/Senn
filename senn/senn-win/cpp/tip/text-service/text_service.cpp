#include "object_releaser.h"
#include "text_service.h"
#include "../ime/stateful_im_proxy_ipc.h"
#include "ui.h"

namespace senn {
namespace senn_win {
namespace text_service {

EditSession::EditSession(ITfCompositionSink *composition_sink,
                         ITfContext *context,
                         ITfComposition **composition,
                         const std::wstring& text,
                         TfGuidAtom editing_atom)
    : composition_sink_(composition_sink),
      context_(context),
      text_(text),
      composition_(composition),
      editing_atom_(editing_atom) {
  context_->AddRef();
}

EditSession::~EditSession() {
  context_->Release();
}

HRESULT __stdcall EditSession::DoEditSession(TfEditCookie ec) {
  // Draw the text on the screen.
  // If it is the first time to draw, we have to create a composition as well.
  ITfRange *range;
  if (*composition_ == nullptr) {
    range = ui::InsertTextAndStartComposition(
        composition_sink_, ec, text_, context_, composition_);
    if (*composition_ == nullptr || range == nullptr) {
      return S_OK;
    }
  } else {
    range = ui::ReplaceTextInComposition(ec, text_, *composition_);
    if (range == nullptr) {
      return S_OK;
    }
  }
  ObjectReleaser<ITfRange> range_releaser(range);

  // Decorate the text with a display attribute of an underline, etc.
  ui::SetDisplayAttribute(ec, context_, range, editing_atom_);

  // Update the selection
  // We'll make it an insertion point just past the inserted text. 
  {
    ITfRange *range_for_selection;
    if (range->Clone(&range_for_selection) == S_OK) {
      range_for_selection->Collapse(ec, TF_ANCHOR_END);

      TF_SELECTION selection;
      selection.range = range_for_selection;
      selection.style.ase = TF_AE_NONE;
      selection.style.fInterimChar = FALSE;
      context_->SetSelection(ec, 1, &selection);

      range_for_selection->Release();
    }
  }
 
  return S_OK;
}


// ITfTextInputProcessor

HRESULT TextService::Activate(ITfThreadMgr *thread_mgr, TfClientId client_id) {
  thread_mgr_ = thread_mgr;
  thread_mgr->AddRef();

  client_id_ = client_id;

  // Create a stateful IM to process user inputs of keys.
  stateful_im_ =
      ::senn::senn_win::ime::StatefulIMProxyIPC::Create(kNamedPipePath);
  if (stateful_im_ == nullptr) {
    return E_FAIL;
  }


  HRESULT result;
  // Advice key event sink to receive key input notifications.
  {
    ITfKeystrokeMgr *keystroke_mgr;
    if (thread_mgr->QueryInterface(
            IID_ITfKeystrokeMgr, (void **)&keystroke_mgr) !=  S_OK) {
      return E_FAIL;
    }

    result = keystroke_mgr->AdviseKeyEventSink(
        client_id, static_cast<ITfKeyEventSink*>(this), TRUE);

    keystroke_mgr->Release();

    if (FAILED(result)) {
      return result;
    }
  }

  // Register guids for display attribute to decorate composing texts.
  {
    ITfCategoryMgr *category_mgr;
    result = CoCreateInstance(
        CLSID_TF_CategoryMgr, nullptr, CLSCTX_INPROC_SERVER,
        IID_ITfCategoryMgr, (void**)&category_mgr);
    if (FAILED(result)) {
      return result;
    }
    ObjectReleaser<ITfCategoryMgr> category_mgr_releaser(category_mgr);

    result = category_mgr->RegisterGUID(
        ui::editing::kDisplayAttributeGuid, &display_attribute_vals_.editing_atom);
    if (FAILED(result)) {
      return result;
    }
  }

  return result;
}

HRESULT TextService::Deactivate() {
  {
    ITfKeystrokeMgr *keystroke_mgr;
    if (thread_mgr_->QueryInterface(
            IID_ITfKeystrokeMgr, (void **)&keystroke_mgr) != S_OK) {
      return S_OK;
    }
    keystroke_mgr->UnadviseKeyEventSink(client_id_);
    keystroke_mgr->Release();
  }

  if (thread_mgr_) {
    thread_mgr_->Release();
    thread_mgr_ = NULL;
  }
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
    REFGUID guid,
    ITfDisplayAttributeInfo **attribute) {
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
    TfEditCookie ecWrite,
    ITfComposition * pComposition) {
  return S_OK;
}


// ITfKeyEventSink

HRESULT __stdcall TextService::OnSetFocus(BOOL fForeground) {
  return S_OK;
}

HRESULT __stdcall TextService::OnTestKeyDown(
    ITfContext *context, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = true;
  return S_OK;
}

HRESULT __stdcall TextService::OnKeyDown(
    ITfContext *context, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  EditSession *edit_session = nullptr;

  *pfEaten = stateful_im_->Input(wParam,
      [&](const std::wstring *text) {
        edit_session = new EditSession(
            this, context, &composition_,
            *text,           
            display_attribute_vals_.editing_atom);
      });
  
  if (edit_session == nullptr) {
    return E_FAIL;
  }
  ObjectReleaser<EditSession> edit_session_releaser(edit_session);

  HRESULT result;
  if (context->RequestEditSession(
          client_id_, edit_session, TF_ES_SYNC | TF_ES_READWRITE, &result) ==
      S_OK) {
    return S_OK;
  } else {
    return E_FAIL;
  }
}

HRESULT __stdcall TextService::OnTestKeyUp(
    ITfContext *context, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT __stdcall TextService::OnKeyUp(
    ITfContext *context, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT __stdcall TextService::OnPreservedKey(
    ITfContext *context, REFGUID rguid, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

} // text_service
} // win
} // senn
