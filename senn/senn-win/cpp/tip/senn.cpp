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


HRESULT __stdcall EditSession::QueryInterface(REFIID riid, void **ppvObject) {
  if (ppvObject == NULL) {
    return E_INVALIDARG;
  }
  if (IsEqualIID(riid, IID_IUnknown) ||
      IsEqualIID(riid, IID_ITfEditSession)) {
    *ppvObject = (ITfLangBarItem *)this;
  } else {
    *ppvObject = NULL;
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


HRESULT __stdcall EditSession::DoEditSession(TfEditCookie ec) {
  ITfInsertAtSelection *insert;
  if (context_->QueryInterface(
          IID_ITfInsertAtSelection, (void **)&insert) !=
      S_OK) {
    return S_OK;
  }

  ITfRange *range;
  if (insert->InsertTextAtSelection(ec, 0, L"a", 1, &range) != S_OK) {
    insert->Release();
    return S_OK;
  }

  range->Collapse(ec, TF_ANCHOR_END);

  TF_SELECTION selection;
  selection.range = range;
  selection.style.ase = TF_AE_NONE;
  selection.style.fInterimChar = FALSE;
  context_->SetSelection(ec, 1, &selection);

  range->Release();

  return S_OK;
}


HRESULT __stdcall TextService::QueryInterface(REFIID riid, void** ppvObject) {
  if (ppvObject == NULL) {
    return E_INVALIDARG;
  }
  if (IsEqualIID(riid, IID_IUnknown) ||
      IsEqualIID(riid, IID_ITfTextInputProcessor)) {
    *ppvObject = (ITfTextInputProcessor *)this;
  } else if (IsEqualIID(riid, IID_ITfKeyEventSink)) {
    *ppvObject = (ITfKeyEventSink *)this;
  } else {
    *ppvObject = NULL;
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


HRESULT TextService::Activate(ITfThreadMgr *thread_mgr, TfClientId client_id) {
  thread_mgr_ = thread_mgr;
  thread_mgr->AddRef();

  client_id_ = client_id;

  return S_OK;
}

HRESULT TextService::Deactivate() {
  ITfKeystrokeMgr *keystroke_mgr;
  if (thread_mgr_->QueryInterface(
          IID_ITfKeystrokeMgr, (void **)&keystroke_mgr) !=
      S_OK) {
    return S_OK;
  }

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
  if (pic->RequestEditSession(
          client_id_, edit_session, TF_ES_SYNC | TF_ES_READWRITE, &hr) !=
      S_OK) {
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
