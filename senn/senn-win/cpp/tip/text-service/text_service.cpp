#include "object_releaser.h"
#include "text_service.h"
#include "../ime/stateful_im_proxy_ipc.h"
#include "ui.h"

namespace senn {
namespace senn_win {
namespace text_service {

EditSessionEditing::EditSessionEditing(
    const senn::senn_win::ime::views::Editing& view,
    ITfContext *context,
    TfGuidAtom display_attribute_atom,
    ITfCompositionSink *composition_sink,
    CompositionHolder *composition_holder)
  : view_(view),
    context_(context),
    display_attribute_atom_(display_attribute_atom),
    composition_sink_(composition_sink),
    composition_holder_(composition_holder) {
  context_->AddRef();
}

EditSessionEditing::~EditSessionEditing() {
  context_->Release();
}

HRESULT __stdcall EditSessionEditing::DoEditSession(TfEditCookie ec) {
  // Draw the text on the screen.
  // If it is the first time to draw, we have to create a composition as well.
  ITfRange *range;
  if (composition_holder_->Get() == nullptr) {
    ITfComposition *composition;
    range = ui::InsertTextAndStartComposition(
        view_.input, ec, context_, composition_sink_, &composition);
    if (composition == nullptr || range == nullptr) {
      return S_OK;
    }
    composition_holder_->Set(composition);
  } else {
    range = ui::ReplaceTextInComposition(
        view_.input, ec, composition_holder_->Get());
    if (range == nullptr) {
      return S_OK;
    }
  }
  ObjectReleaser<ITfRange> range_releaser(range);

  // Decorate the text with a display attribute of an underline, etc.
  ui::SetDisplayAttribute(ec, context_, range, display_attribute_atom_);

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

EditSessionCommitted::EditSessionCommitted(
    const senn::senn_win::ime::views::Committed& view,
    ITfContext *context,
    CompositionHolder *composition_holder)
  : view_(view),
    context_(context),
    composition_holder_(composition_holder) {
  context_->AddRef();
}

EditSessionCommitted::~EditSessionCommitted() {
  context_->Release();
}

HRESULT __stdcall EditSessionCommitted::DoEditSession(TfEditCookie ec) {
  if (composition_holder_->Get() != nullptr) {
    ITfComposition *composition = composition_holder_->Get();
    ui::ReplaceTextInComposition(view_.input, ec, composition);
    ui::RemoveDisplayAttributes(ec, context_, composition);

    composition->EndComposition(ec);
    composition->Release();
    composition_holder_->Set(nullptr);
  }
  context_->Release();
  return S_OK;
}


// ITfTextInputProcessor

HRESULT TextService::Activate(ITfThreadMgr *thread_mgr, TfClientId client_id) {
  thread_mgr_ = thread_mgr;
  thread_mgr->AddRef();

  client_id_ = client_id;

  // Create a stateful IM to process user inputs of keys.
  stateful_im_ =
      senn::senn_win::ime::StatefulIMProxyIPC::Create(kNamedPipePath);
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
        ui::editing::kDisplayAttributeGuid, &editing_display_attribute_atom_);
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
  *pfEaten = true;

  ITfEditSession *edit_session = nullptr;
  stateful_im_->Input(wParam,
      [&](const senn::senn_win::ime::views::Editing& view) {
        edit_session = new EditSessionEditing(
            view, context, editing_display_attribute_atom_, 
            this, &composition_holder_);
      },
      [&](const senn::senn_win::ime::views::Committed& view) {
        edit_session = new EditSessionCommitted(
            view, context, &composition_holder_);
      });
  if (edit_session == nullptr) {
    return E_FAIL;
  }
  ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);

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