#include "key_event_handler.h"
#include "candidate_window.h"
#include "object_releaser.h"
#include "ui.h"

namespace senn {
namespace senn_win {
namespace text_service {

EditSessionEditing::EditSessionEditing(
    const senn::win::im::views::Editing &view, ITfContext *context,
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
  if (view_.input.empty()) {
    if (composition_holder_->Get() == nullptr) {
      return S_OK;
    }
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
    return S_OK;
  }

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
    ITfThreadMgr *thread_mgr, const senn::win::im::views::Converting &view,
    ITfContext *context, const DisplayAttributeAtoms *atoms,
    ITfComposition *composition)
    : thread_mgr_(thread_mgr), view_(view), context_(context), atoms_(atoms),
      composition_(composition) {
  context_->AddRef();
  thread_mgr_->AddRef();
}

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
    const senn::win::im::views::Committed &view, ITfContext *context,
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

HRESULT __stdcall MoveCandidateWindowToTextPositionEditSession::DoEditSession(
    TfEditCookie ec) {
  ITfRange *range = nullptr;
  if (composition_->GetRange(&range) != S_OK || range == nullptr) {
    // Can't do anything...
    return S_OK;
  }
  ObjectReleaser<ITfRange> range_releaser(range);

  RECT rc = {};
  BOOL fClipped;
  if (context_view_->GetTextExt(ec, range, &rc, &fClipped) == S_OK) {
    ui_->Move(&rc);
  }
  return S_OK;
}

HRESULT
KeyEventHandler::HandleIMEView(ITfContext *context,
                               const senn::win::im::views::Editing &view) {
  ITfEditSession *edit_session =
      new EditSessionEditing(view, context, editing_display_attribute_atom_,
                             composition_sink_, &composition_holder_);
  ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);

  if (!candidate_list_ui_) {
    candidate_list_state_ = new CandidateListState();
    candidate_list_ui_ =
        CandidateListUI::Create(context, thread_mgr_, candidate_list_state_,
                                static_cast<CandidateListUI::Handlers *>(this));
  }

  candidate_list_state_->Update(view);
  candidate_list_ui_->NotifyUpdateUI();

  HRESULT result;
  return context->RequestEditSession(client_id_, edit_session,
                                     TF_ES_SYNC | TF_ES_READWRITE, &result);
}

HRESULT
KeyEventHandler::HandleIMEView(ITfContext *context,
                               const senn::win::im::views::Converting &view) {
  ITfEditSession *edit_session = new EditSessionConverting(
      thread_mgr_, view, context, converting_display_attribute_atoms_,
      composition_holder_.Get());
  ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);

  HRESULT result, ret;
  ret = context->RequestEditSession(client_id_, edit_session,
                                    TF_ES_SYNC | TF_ES_READWRITE, &result);

  if (ret != S_OK) {
    return ret;
  }

  if (!candidate_list_ui_) {
    candidate_list_state_ = new CandidateListState();
    candidate_list_ui_ =
        CandidateListUI::Create(context, thread_mgr_, candidate_list_state_,
                                static_cast<CandidateListUI::Handlers *>(this));
  }

  candidate_list_state_->Update(view);
  candidate_list_ui_->NotifyUpdateUI();

  return S_OK;
}

HRESULT
KeyEventHandler::HandleIMEView(ITfContext *context,
                               const senn::win::im::views::Committed &view) {
  if (candidate_list_ui_) {
    delete candidate_list_state_;
    candidate_list_state_ = nullptr;

    // If DestroyUI is called from the destructor, the process crashes...
    candidate_list_ui_->DestroyUI();
    candidate_list_ui_->Release();
    candidate_list_ui_ = nullptr;
  }

  ITfEditSession *edit_session = new EditSessionCommitted(
      view, context, composition_sink_, &composition_holder_);

  HRESULT result;
  return context->RequestEditSession(client_id_, edit_session,
                                     TF_ES_SYNC | TF_ES_READWRITE, &result);
}

KeyEventHandler::KeyEventHandler(
    ITfThreadMgr *thread_mgr, TfClientId id, ITfCompositionSink *sink,
    senn::win::im::StatefulIME *ime, TfGuidAtom atom_editing,
    EditSessionConverting::DisplayAttributeAtoms *atoms_converting,
    Handlers *handlers)
    : thread_mgr_(thread_mgr), client_id_(id), composition_sink_(sink),
      ime_(ime), editing_display_attribute_atom_(atom_editing),
      converting_display_attribute_atoms_(atoms_converting),
      handlers_(handlers), candidate_list_state_(nullptr),
      candidate_list_ui_(nullptr) {}

KeyEventHandler::~KeyEventHandler() {
  if (candidate_list_state_) {
    delete candidate_list_state_;
  }

  if (candidate_list_ui_) {
    candidate_list_ui_->DestroyUI();
    delete candidate_list_ui_;
  }
}

HRESULT KeyEventHandler::OnLayoutChange(ITfContext *pic,
                                        ITfContextView *pView) {
  ITfComposition *composition = composition_holder_.Get();
  if (!composition) {
    // Can't do anything...
    return S_OK;
  }

  ITfEditSession *edit_session =
      new MoveCandidateWindowToTextPositionEditSession(pView, composition,
                                                       candidate_list_ui_);
  ObjectReleaser<ITfEditSession> releaser(edit_session);
  HRESULT hr;
  return pic->RequestEditSession(client_id_, edit_session,
                                 TF_ES_SYNC | TF_ES_READ, &hr);
}

HRESULT KeyEventHandler::OnSetFocus(BOOL fForeground) { return S_OK; }

HRESULT KeyEventHandler::OnTestKeyDown(ITfContext *context, WPARAM wParam,
                                       LPARAM lParam, BOOL *pfEaten) {
  if (wParam == 0xF3 || wParam == 0xF4) {
    // hankaku/zenkaku key
    handlers_->OnToggleInputModeKeyDown();
    *pfEaten = false;
    return S_OK;
  }

  *pfEaten = ime_->CanProcess(wParam);
  return S_OK;
}

HRESULT KeyEventHandler::OnKeyDown(ITfContext *context, WPARAM wParam,
                                   LPARAM lParam, BOOL *pfEaten) {
  if (wParam == 0xF3 || wParam == 0xF4) {
    // hankaku/zenkaku key
    handlers_->OnToggleInputModeKeyDown();
    *pfEaten = false;
    return S_OK;
  }

  BYTE key_state[256] = {};
  if (!GetKeyboardState(key_state)) {
    *pfEaten = false;
    return S_OK;
  }

  HRESULT result;
  *pfEaten = ime_->ProcessInput(
      wParam, key_state,
      [&](const senn::win::im::views::Editing &view) {
        result = HandleIMEView(context, view);
      },
      [&](const senn::win::im::views::Converting &view) {
        result = HandleIMEView(context, view);
      },
      [&](const senn::win::im::views::Committed &view) {
        result = HandleIMEView(context, view);
      });
  return result;
}

HRESULT KeyEventHandler::OnTestKeyUp(ITfContext *context, WPARAM wParam,
                                     LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT KeyEventHandler::OnKeyUp(ITfContext *context, WPARAM wParam,
                                 LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT KeyEventHandler::OnPreservedKey(ITfContext *context, REFGUID rguid,
                                        BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

namespace {

bool IsSameCandidateList(const std::vector<std::wstring> &c1,
                         const std::vector<std::wstring> &c2) {

  if (c1.size() != c2.size()) {
    return false;
  }

  bool is_same = true;
  for (size_t i = 0; is_same && i < c1.size(); i++) {
    is_same &= (c1[i] == c2[i]);
  }
  return is_same;
}

} // namespace

void CandidateListState::Update(const senn::win::im::views::Editing &view) {
  current_index_ = -1;

  if (!IsSameCandidateList(candidates_, view.predictions)) {
    candidates_.clear();
    for (std::vector<std::wstring>::const_iterator it =
             view.predictions.begin();
         it != view.predictions.end(); ++it) {
      candidates_.push_back(*it);
    }
  }
}

void CandidateListState::Update(const senn::win::im::views::Converting &view) {
  // Update candidate index
  current_index_ = view.cursor_form_candidate_index;

  // Update candidates
  // If the updated candidate list is the same as the current one, we don't
  // update.
  if (!IsSameCandidateList(candidates_, view.cursor_form_candidates)) {
    candidates_.clear();
    for (std::vector<std::wstring>::const_iterator it =
             view.cursor_form_candidates.begin();
         it != view.cursor_form_candidates.end(); ++it) {
      candidates_.push_back(*it);
    }
  }
}

} // namespace text_service
} // namespace senn_win
} // namespace senn