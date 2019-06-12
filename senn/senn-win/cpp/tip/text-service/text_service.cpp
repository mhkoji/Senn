#include "object_releaser.h"
#include "text_service.h"
#include "langbar.h"
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


EditSessionConverting::EditSessionConverting(
    const senn::senn_win::ime::views::Converting& view,
    ITfContext *context,
    const DisplayAttributeAtoms& atoms,
    ITfComposition *composition)
  : view_(view),
    context_(context),
    atoms_(atoms),
    composition_(composition) {
  context_->AddRef();
}

EditSessionConverting::~EditSessionConverting() {
  context_->Release();
}


HRESULT __stdcall EditSessionConverting::DoEditSession(TfEditCookie ec) {
  // Composition must have started by the previous EditSessionCommitted
  if (composition_ == nullptr) {
    return S_OK;
  }

  // Draw the current converted text
  std::wstring text = L"";
  for (size_t i = 0; i < view_.forms.size(); ++i) {
    text += view_.forms[i];
  }

  ITfRange *range = ui::ReplaceTextInComposition(text, ec, composition_); 
  if (range == nullptr) {
    return S_OK;
  }
  ObjectReleaser<ITfRange> range_releaser(range);

  // Decorate the text
  {
    LONG start = 0;
    for (size_t i = 0; i < view_.forms.size(); ++i) {
      ITfRange *segment_range;
      range->Clone(&segment_range);
      ObjectReleaser<ITfRange> segment_range_releaser(segment_range);

      HRESULT result;
      result = segment_range->Collapse(ec, TF_ANCHOR_START);

      LONG end = start + view_.forms[i].length();
      LONG shift = 0;
      result = segment_range->ShiftEnd(ec, end, &shift, nullptr);
      if (FAILED(result)) {
        return result;
      }
      result = segment_range->ShiftStart(ec, start, &shift, nullptr);
      if (FAILED(result)) {
        return result;
      }

      TfGuidAtom attr = (i == view_.cursor_form_index) ?
          atoms_.focused :
          atoms_.non_focused;
      ui::SetDisplayAttribute(ec, context_, segment_range, attr);

      start = end;
    }
  }

  return S_OK;
}


EditSessionCommitted::EditSessionCommitted(
    const senn::senn_win::ime::views::Committed& view,
    ITfContext *context,
    ITfCompositionSink* composition_sink,
    CompositionHolder *composition_holder)
  : view_(view),
    context_(context),
    composition_sink_(composition_sink),
    composition_holder_(composition_holder) {
  context_->AddRef();
}

EditSessionCommitted::~EditSessionCommitted() {
  context_->Release();
}

HRESULT __stdcall EditSessionCommitted::DoEditSession(TfEditCookie ec) {
  if (composition_holder_->Get() == nullptr) {
    ITfComposition *composition;
    ITfRange *range = ui::InsertTextAndStartComposition(
        view_.input, ec, context_, composition_sink_, &composition);
    if (range != nullptr) {
      range->Release();
    }
    if (composition != nullptr) {
      composition->EndComposition(ec);
      composition->Release();
    }
  } else {
    ITfComposition *composition = composition_holder_->Get();
    ITfRange *range = ui::ReplaceTextInComposition(
        view_.input, ec, composition);
    if (range != nullptr) {
      ui::RemoveDisplayAttributes(ec, context_, range);
      range->Release();
    }
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
  // Add language bar items.
  {
    ITfLangBarItemMgr *lang_bar_item_mgr;
    if (thread_mgr->QueryInterface(
            IID_ITfLangBarItemMgr, (void **)&lang_bar_item_mgr) != S_OK) {
      return E_FAIL;
    }
    input_mode_toggle_button_ =
        new langbar::InputModeToggleButton(clsid_text_service_, 0);
    lang_bar_item_mgr->AddItem(input_mode_toggle_button_);

    lang_bar_item_mgr->Release();
  }

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

    struct {
      const GUID& guid;
      TfGuidAtom *output;
    } settings[] = {
      {
        ui::editing::kDisplayAttributeGuid,
        &editing_display_attribute_atom_
      },
      {
        ui::converting::non_focused::kDisplayAttributeGuid,
        &converting_display_attribute_atoms_.non_focused
      },
      {
        ui::converting::focused::kDisplayAttributeGuid,
        &converting_display_attribute_atoms_.focused
      }
    };

    for (size_t i = 0; i < ARRAYSIZE(settings); i++) {
      result = category_mgr->RegisterGUID(settings[i].guid,
                                          settings[i].output);
      if (FAILED(result)) {
        return result;
      }
    }
  }

  return result;
}

HRESULT TextService::Deactivate() {
  if (thread_mgr_ == nullptr) {
    return S_OK;
  }

  // Remove language bar items.
  {
    ITfLangBarItemMgr *lang_bar_item_mgr;
    if (thread_mgr_->QueryInterface(
            IID_ITfLangBarItemMgr, (void **)&lang_bar_item_mgr) != S_OK) {
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
    if (thread_mgr_->QueryInterface(
            IID_ITfKeystrokeMgr, (void **)&keystroke_mgr) != S_OK) {
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
  if (wParam == VK_BACK || wParam == VK_LEFT || wParam ==  VK_UP ||
      wParam == VK_RIGHT || wParam == VK_DOWN) {
    // Force the OS operate according to the key.
    *pfEaten = false;
  } else {
    *pfEaten = true;
  }
  return S_OK;
}

HRESULT __stdcall TextService::OnKeyDown(
    ITfContext *context, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = true;

  ITfEditSession *edit_session = nullptr;
  stateful_im_->Transit(wParam,
      [&](const senn::senn_win::ime::views::Editing& view) {
        edit_session = new EditSessionEditing(
            view, context, editing_display_attribute_atom_, 
            this, &composition_holder_);
      },
      [&](const senn::senn_win::ime::views::Converting& view) {
        edit_session = new EditSessionConverting(
            view, context, converting_display_attribute_atoms_,
            composition_holder_.Get());
      },
      [&](const senn::senn_win::ime::views::Committed& view) {
        edit_session = new EditSessionCommitted(
            view, context, this, &composition_holder_);
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
