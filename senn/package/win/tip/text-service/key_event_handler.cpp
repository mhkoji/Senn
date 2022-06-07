#include "key_event_handler.h"
#include "candidate_window.h"
#include "object_releaser.h"
#include "ui.h"

namespace senn {
namespace senn_win {
namespace text_service {

EditSessionEditing::EditSessionEditing(
    const senn::win::im::views::Editing &view, ITfContext *context,
    ITfThreadMgr *thread_mgr, TfClientId client_id,
    TfGuidAtom display_attribute_atom, ITfCompositionSink *composition_sink,
    CompositionHolder *composition_holder,
    CandidateListHolder *candidate_list_holder)
    : view_(view), context_(context), thread_mgr_(thread_mgr),
      client_id_(client_id), display_attribute_atom_(display_attribute_atom),
      composition_sink_(composition_sink),
      composition_holder_(composition_holder),
      candidate_list_holder_(candidate_list_holder) {
  context_->AddRef();
}

EditSessionEditing::~EditSessionEditing() { context_->Release(); }

HRESULT __stdcall EditSessionEditing::DoEditSession(TfEditCookie ec) {
  if (view_.input.empty()) {
    ITfComposition *composition = composition_holder_->Get();
    if (composition == nullptr) {
      return S_OK;
    }

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

  if (candidate_list_holder_->Get() == nullptr) {
    candidate_list_holder_->Set(CandidateList::Create(
        context_, thread_mgr_, composition_holder_->Get(), client_id_));
  }
  candidate_list_holder_->Get()->Update(view_);

  return S_OK;
}

EditSessionConverting::EditSessionConverting(
    const senn::win::im::views::Converting &view, ITfContext *context,
    ITfThreadMgr *thread_mgr, TfClientId client_id,
    const DisplayAttributeAtoms *atoms, ITfComposition *composition,
    CandidateListHolder *candidate_list_holder)
    : view_(view), context_(context), thread_mgr_(thread_mgr),
      client_id_(client_id), atoms_(atoms), composition_(composition),
      candidate_list_holder_(candidate_list_holder) {
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

  if (candidate_list_holder_->Get() == nullptr) {
    candidate_list_holder_->Set(
        CandidateList::Create(context_, thread_mgr_, composition_, client_id_));
  }
  candidate_list_holder_->Get()->Update(view_);

  return S_OK;
}

EditSessionCommitted::EditSessionCommitted(
    const senn::win::im::views::Committed &view, ITfContext *context,
    ITfCompositionSink *composition_sink, CompositionHolder *composition_holder,
    CandidateListHolder *candidate_list_holder)
    : view_(view), context_(context), composition_sink_(composition_sink),
      composition_holder_(composition_holder),
      candidate_list_holder_(candidate_list_holder) {
  context_->AddRef();
}

EditSessionCommitted::~EditSessionCommitted() { context_->Release(); }

HRESULT __stdcall EditSessionCommitted::DoEditSession(TfEditCookie ec) {
  {
    CandidateList *candidate_list = candidate_list_holder_->Get();
    if (candidate_list != nullptr) {
      delete candidate_list;
    }
    candidate_list_holder_->Set(nullptr);
  }

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

KeyEventHandler::KeyEventHandler(
    ITfThreadMgr *thread_mgr, TfClientId id, ITfCompositionSink *sink,
    senn::win::im::StatefulIME *ime, TfGuidAtom atom_editing,
    EditSessionConverting::DisplayAttributeAtoms *atoms_converting,
    Handlers *handlers)
    : thread_mgr_(thread_mgr), client_id_(id), composition_sink_(sink),
      ime_(ime), editing_display_attribute_atom_(atom_editing),
      converting_display_attribute_atoms_(atoms_converting),
      handlers_(handlers) {}

KeyEventHandler::~KeyEventHandler() {
  CandidateList *candidate_list = candidate_list_holder_.Get();
  if (candidate_list != nullptr) {
    delete candidate_list;
  }
  candidate_list_holder_.Set(nullptr);
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

  HRESULT result = S_OK;
  *pfEaten = ime_->ProcessInput(
      wParam, key_state,
      [&](const senn::win::im::views::Editing &view) {
        ITfEditSession *edit_session = new EditSessionEditing(
            view, context, thread_mgr_, client_id_,
            editing_display_attribute_atom_, composition_sink_,
            &composition_holder_, &candidate_list_holder_);
        ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);
        context->RequestEditSession(client_id_, edit_session,
                                    TF_ES_SYNC | TF_ES_READWRITE, &result);
      },
      [&](const senn::win::im::views::Converting &view) {
        ITfEditSession *edit_session = new EditSessionConverting(
            view, context, thread_mgr_, client_id_,
            converting_display_attribute_atoms_, composition_holder_.Get(),
            &candidate_list_holder_);
        ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);
        context->RequestEditSession(client_id_, edit_session,
                                    TF_ES_SYNC | TF_ES_READWRITE, &result);
      },
      [&](const senn::win::im::views::Committed &view) {
        ITfEditSession *edit_session = new EditSessionCommitted(
            view, context, composition_sink_, &composition_holder_,
            &candidate_list_holder_);
        ObjectReleaser<ITfEditSession> edit_session_releaser(edit_session);
        context->RequestEditSession(client_id_, edit_session,
                                    TF_ES_SYNC | TF_ES_READWRITE, &result);
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

CandidateList *CandidateList::Create(ITfContext *context,
                                     ITfThreadMgr *thread_mgr,
                                     ITfComposition *composition,
                                     TfClientId client_id) {
  CandidateList *candidate_list = new CandidateList(composition, client_id);
  candidate_list->ui_ = CandidateListUI::Create(
      context, thread_mgr, &candidate_list->state_,
      static_cast<CandidateListUI::Handlers *>(candidate_list));
  return candidate_list;
}

CandidateList::CandidateList(ITfComposition *composition, TfClientId client_id)
    : ui_(nullptr), composition_(composition), client_id_(client_id) {}

void CandidateList::Update(const senn::win::im::views::Editing &view) {
  state_.current_index_ = -1;

  if (!IsSameCandidateList(state_.candidates_, view.predictions)) {
    state_.candidates_.clear();
    for (std::vector<std::wstring>::const_iterator it =
             view.predictions.begin();
         it != view.predictions.end(); ++it) {
      state_.candidates_.push_back(*it);
    }
  }

  ui_->NotifyUpdateUI();
}

void CandidateList::Update(const senn::win::im::views::Converting &view) {
  // Update candidate index
  state_.current_index_ = view.cursor_form_candidate_index;

  // Update candidates
  // If the updated candidate list is the same as the current one, we don't
  // update.
  if (!IsSameCandidateList(state_.candidates_, view.cursor_form_candidates)) {
    state_.candidates_.clear();
    for (std::vector<std::wstring>::const_iterator it =
             view.cursor_form_candidates.begin();
         it != view.cursor_form_candidates.end(); ++it) {
      state_.candidates_.push_back(*it);
    }
  }

  ui_->NotifyUpdateUI();
}

CandidateList::~CandidateList() {
  // If DestroyUI is called from the destructor, the process crashes...
  ui_->DestroyUI();
  ui_->Release();
  ui_ = nullptr;
}

HRESULT CandidateList::OnLayoutChange(ITfContext *pic, ITfContextView *pView) {
  if (!composition_) {
    // Can't do anything...
    return S_OK;
  }

  ITfEditSession *edit_session =
      new MoveCandidateWindowToTextPositionEditSession(pView, composition_,
                                                       ui_);
  ObjectReleaser<ITfEditSession> releaser(edit_session);
  HRESULT hr;
  return pic->RequestEditSession(client_id_, edit_session,
                                 TF_ES_SYNC | TF_ES_READ, &hr);
}

} // namespace text_service
} // namespace senn_win
} // namespace senn