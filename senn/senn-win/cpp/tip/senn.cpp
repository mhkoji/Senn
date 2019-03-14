#include "stdafx.h"

#include <vector>
#include <cstdlib>
#include <msctf.h>
#include <initguid.h>

#include "senn.h"
#include <iostream>

namespace senn {
namespace senn_win {

namespace text_service { /////////////////////////////////////////////////////////
  
EditSession::EditSession(ITfContext *context) : context_(context) {
}

EditSession::~EditSession() {
  context_->Release();
}


HRESULT __stdcall EditSession::QueryInterface(REFIID riid, void** ppvObject) {
  if (ppvObject == NULL) {
    return E_INVALIDARG;
  }

  *ppvObject = CastSelf(this, riid);
  if (*ppvObject == NULL) {
    return E_NOINTERFACE;
  }
  AddRef();

  return S_OK;
}

ULONG __stdcall EditSession::AddRef(void) {
  return ++ref_count_;
}

ULONG __stdcall EditSession::Release(void) {
  if (ref_count_ <= 0) {
    return 0;
  }

  const ULONG count = --ref_count_;
  if (count == 0) {
    delete this;
  }
  return count;
}

void* EditSession::CastSelf(EditSession *self, REFIID riid) {
  if (IsEqualIID(riid, IID_IUnknown) || IsEqualIID(riid, IID_ITfEditSession)) {
    return (ITfLangBarItem *)self;
  }
  return NULL;
}


HRESULT __stdcall EditSession::DoEditSession(TfEditCookie ec) {
  ITfInsertAtSelection *insert_at_selection;
  if (context_->QueryInterface(IID_ITfInsertAtSelection,
                               (void **)&insert_at_selection) != S_OK) {
    return S_OK;
  }

  ITfRange *pRange;
  if (insert_at_selection->InsertTextAtSelection(ec, 0, L"a", 1, &pRange) != S_OK) {
    insert_at_selection->Release();
    return S_OK;
  }

  pRange->Collapse(ec, TF_ANCHOR_END);

  TF_SELECTION tfSelection;
  tfSelection.range = pRange;
  tfSelection.style.ase = TF_AE_NONE;
  tfSelection.style.fInterimChar = FALSE;

  context_->SetSelection(ec, 1, &tfSelection);

  pRange->Release();

  return S_OK;
}


HRESULT __stdcall TextService::QueryInterface(REFIID riid, void** ppvObject) {
  if (ppvObject == NULL) {
    return E_INVALIDARG;
  }

  *ppvObject = CastSelf(this, riid);
  if (*ppvObject == NULL) {
    return E_NOINTERFACE;
  }
  AddRef();

  return S_OK;
}

ULONG __stdcall TextService::AddRef(void) {
  return ++ref_count_;
}

ULONG __stdcall TextService::Release(void) {
  if (ref_count_ <= 0) {
    return 0;
  }

  const ULONG count = --ref_count_;
  if (count == 0) {
    delete this;
  }
  return count;
}

void* TextService::CastSelf(TextService *self, REFIID riid) {
  if (IsEqualIID(riid, IID_IUnknown) ||
      IsEqualIID(riid, IID_ITfTextInputProcessor)) {
    return (ITfTextInputProcessor *)self;
  }
  if (IsEqualIID(riid, IID_ITfKeyEventSink)) {
    return (ITfKeyEventSink *)self;
  }
  return NULL;
}


HRESULT TextService::Activate(ITfThreadMgr *thread_mgr, TfClientId client_id) {
  thread_mgr_ = thread_mgr;
  thread_mgr->AddRef();

  client_id_ = client_id;

  ITfKeystrokeMgr *keystroke_mgr;
  if (thread_mgr->QueryInterface(IID_ITfKeystrokeMgr, (void **)&keystroke_mgr)
      != S_OK) {
    return FALSE;
  }

  HRESULT result = keystroke_mgr->AdviseKeyEventSink(
      client_id, (ITfKeyEventSink*)this, TRUE);

  keystroke_mgr->Release();

  return result == S_OK;
}

HRESULT TextService::Deactivate() {
  ITfKeystrokeMgr *keystroke_mgr;
  if (thread_mgr_->QueryInterface(IID_ITfKeystrokeMgr,
                                  (void **)&keystroke_mgr) !=
      S_OK) {
    return S_OK;
  }
  keystroke_mgr->UnadviseKeyEventSink(client_id_);
  keystroke_mgr->Release();

  if (thread_mgr_) {
    thread_mgr_->Release();
    thread_mgr_ = NULL;
  }
  return S_OK;
}


HRESULT __stdcall TextService::OnSetFocus(BOOL fForeground) {
  return S_OK;
}

HRESULT __stdcall TextService::OnTestKeyDown(ITfContext *pic, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  return S_OK;
}

HRESULT __stdcall TextService::OnTestKeyUp(ITfContext *pic, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  return S_OK;
}

HRESULT __stdcall TextService::OnKeyDown(ITfContext *pic, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  ITfEditSession *edit_session = new EditSession(pic);
  HRESULT hr = S_OK;
  if (pic->RequestEditSession(client_id_,
                              edit_session,
                              TF_ES_SYNC | TF_ES_READWRITE,
                              &hr) != S_OK) {
      hr = E_FAIL;
  }

  edit_session->Release();
  return S_OK;
}

HRESULT __stdcall TextService::OnKeyUp(ITfContext *pic, WPARAM wParam, LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT __stdcall TextService::OnPreservedKey(ITfContext *pic, REFGUID rguid, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

} // text_service /////////////////////////////////////////////////////////

} // win
} // senn
