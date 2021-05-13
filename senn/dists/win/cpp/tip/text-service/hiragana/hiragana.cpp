#include "hiragana.h"
#include "../object_releaser.h"
#include "candidate_window.h"
#include "ui.h"
#include <cassert>

namespace senn {
namespace senn_win {
namespace text_service {
namespace hiragana {

EditSessionEditing::EditSessionEditing(
    const senn::senn_win::ime::views::Editing &view, ITfContext *context,
    TfGuidAtom display_attribute_atom, ITfCompositionSink *composition_sink,
    CompositionHolder *composition_holder)
    : view_(view), context_(context),
      display_attribute_atom_(display_attribute_atom),
      composition_sink_(composition_sink),
      composition_holder_(composition_holder) {
  context_->AddRef();
}

EditSessionEditing::~EditSessionEditing() { context_->Release(); }

HRESULT __stdcall EditSessionEditing::DoEditSession(TfEditCookie ec) {
  // Draw the text on the screen.
  // If it is the first time to draw, we have to create a composition as well.
  ITfRange *range;
  if (composition_holder_->Get() == nullptr) {
    ITfComposition *composition;
    range = ui::InsertTextAndStartComposition(view_.input, ec, context_,
                                              composition_sink_, &composition);
    if (composition == nullptr || range == nullptr) {
      return S_OK;
    }
    composition_holder_->Set(composition);
  } else {
    range = ui::ReplaceTextInComposition(view_.input, ec,
                                         composition_holder_->Get());
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
    ITfThreadMgr *thread_mgr,
    const senn::senn_win::ime::views::Converting &view, ITfContext *context,
    const DisplayAttributeAtoms *atoms, ITfComposition *composition)
    : thread_mgr_(thread_mgr), view_(view), context_(context), atoms_(atoms),
      composition_(composition) {
  context_->AddRef();
  thread_mgr_->AddRef();
}

CandidateListUI *cw = nullptr;

EditSessionConverting::~EditSessionConverting() {
  context_->Release();
  thread_mgr_->Release();
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

      LONG end = start + static_cast<LONG>(view_.forms[i].length());
      LONG shift = 0;
      result = segment_range->ShiftEnd(ec, end, &shift, nullptr);
      if (FAILED(result)) {
        return result;
      }
      result = segment_range->ShiftStart(ec, start, &shift, nullptr);
      if (FAILED(result)) {
        return result;
      }

      TfGuidAtom attr = (i == view_.cursor_form_index) ? atoms_->focused
                                                       : atoms_->non_focused;
      ui::SetDisplayAttribute(ec, context_, segment_range, attr);

      start = end;
    }
  }

  return S_OK;
}

EditSessionCommitted::EditSessionCommitted(
    const senn::senn_win::ime::views::Committed &view, ITfContext *context,
    ITfCompositionSink *composition_sink, CompositionHolder *composition_holder)
    : view_(view), context_(context), composition_sink_(composition_sink),
      composition_holder_(composition_holder) {
  context_->AddRef();
}

EditSessionCommitted::~EditSessionCommitted() { context_->Release(); }

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
    ITfRange *range =
        ui::ReplaceTextInComposition(view_.input, ec, composition);
    if (range != nullptr) {
      ui::RemoveDisplayAttributes(ec, context_, range);
      range->Release();
    }
    composition->EndComposition(ec);
    composition->Release();
    composition_holder_->Set(nullptr);
  }
  return S_OK;
}

HiraganaKeyEventHandler::HiraganaKeyEventHandler(
    ITfThreadMgr *thread_mgr, TfClientId id, ITfCompositionSink *sink,
    ::senn::senn_win::ime::StatefulIM *im, TfGuidAtom atom_editing,
    EditSessionConverting::DisplayAttributeAtoms *atoms_converting)
    : thread_mgr_(thread_mgr), client_id_(id), composition_sink_(sink),
      stateful_im_(im), editing_display_attribute_atom_(atom_editing),
      converting_display_attribute_atoms_(atoms_converting),
      candidate_list_ui_(nullptr) {}

HRESULT HiraganaKeyEventHandler::OnSetFocus(BOOL fForeground) { return S_OK; }

HRESULT HiraganaKeyEventHandler::OnTestKeyDown(ITfContext *context,
                                               WPARAM wParam, LPARAM lParam,
                                               BOOL *pfEaten) {
  if (wParam == VK_BACK || wParam == VK_LEFT || wParam == VK_UP ||
      wParam == VK_RIGHT || wParam == VK_DOWN) {
    // Force the OS operate according to the key.
    *pfEaten = false;
  } else {
    *pfEaten = true;
  }
  return S_OK;
}

bool HiraganaKeyEventHandler::HandleView(
    ITfContext *context, const senn::senn_win::ime::views::Editing &view) {
  ITfEditSession *edit_session =
      new EditSessionEditing(view, context, editing_display_attribute_atom_,
                             composition_sink_, &composition_holder_);
  ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);

  HRESULT result;
  return context->RequestEditSession(client_id_, edit_session,
                                     TF_ES_SYNC | TF_ES_READWRITE,
                                     &result) == S_OK;
}

bool HiraganaKeyEventHandler::HandleView(
    ITfContext *context, const senn::senn_win::ime::views::Converting &view) {
  ITfEditSession *edit_session = new EditSessionConverting(
      thread_mgr_, view, context, converting_display_attribute_atoms_,
      composition_holder_.Get());
  ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);

  HRESULT result, ret;
  ret = context->RequestEditSession(client_id_, edit_session,
                                    TF_ES_SYNC | TF_ES_READWRITE, &result);

  if (ret != S_OK) {
    return false;
  }

  if (!candidate_list_ui_) {
    candidate_list_ui_ = CandidateListUI::Create(context, thread_mgr_);
  }

  if (0 < view.cursor_form_candidates.size()) {
    candidate_list_ui_->UpdateCandidates(view);
    candidate_list_ui_->Show(true);
  } else {
    candidate_list_ui_->ClearCandidates();
    candidate_list_ui_->Show(false);
  }

  return true;
}

bool HiraganaKeyEventHandler::HandleView(
    ITfContext *context, const senn::senn_win::ime::views::Committed &view) {
  if (candidate_list_ui_) {
    candidate_list_ui_->Release();
    candidate_list_ui_ = nullptr;
  }

  ITfEditSession *edit_session = new EditSessionCommitted(
      view, context, composition_sink_, &composition_holder_);

  HRESULT result;
  return context->RequestEditSession(client_id_, edit_session,
                                     TF_ES_SYNC | TF_ES_READWRITE,
                                     &result) == S_OK;
}

HRESULT HiraganaKeyEventHandler::OnKeyDown(ITfContext *context, WPARAM wParam,
                                           LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = true;

  bool success;
  stateful_im_->Transit(
      wParam,
      [&](const senn::senn_win::ime::views::Editing &view) {
        success = HandleView(context, view);
      },
      [&](const senn::senn_win::ime::views::Converting &view) {
        success = HandleView(context, view);
      },
      [&](const senn::senn_win::ime::views::Committed &view) {
        success = HandleView(context, view);
      });
  return success ? S_OK : E_FAIL;
}

HRESULT HiraganaKeyEventHandler::OnTestKeyUp(ITfContext *context, WPARAM wParam,
                                             LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT HiraganaKeyEventHandler::OnKeyUp(ITfContext *context, WPARAM wParam,
                                         LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT HiraganaKeyEventHandler::OnPreservedKey(ITfContext *context,
                                                REFGUID rguid, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

} // namespace hiragana
} // namespace text_service
} // namespace senn_win
} // namespace senn