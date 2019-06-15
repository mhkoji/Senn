#include <atlbase.h>
#include <strsafe.h>

#include "langbar.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace langbar {

InputModeToggleButton::InputModeToggleButton(
    CLSID clsid,
    ULONG sort,
    State* state,
    Handlers* handlers)
  : ref_count_(1), clsid_(clsid), sort_(sort),
    lang_bar_item_sink_(nullptr),
    state_(state), handlers_(handlers) {
}

HRESULT __stdcall InputModeToggleButton::GetInfo(TF_LANGBARITEMINFO *pInfo) {
  if (!pInfo) {
    return E_INVALIDARG;
  }
  pInfo->clsidService = clsid_;
  pInfo->guidItem = langbar::kItemId;
  pInfo->dwStyle = TF_LBI_STYLE_BTN_BUTTON
  // TF_LBI_STYLE_SHOWNINTRAY seems to enable the item shown in the taskbar.
  // (But, this doesn't work...)
      | TF_LBI_STYLE_SHOWNINTRAY;
  pInfo->ulSort = sort_;
  StringCchCopy(pInfo->szDescription,
                ARRAYSIZE(pInfo->szDescription),
                kItemDescription);
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::GetStatus(DWORD *pdwStatus) {
  if (!pdwStatus) {
    return E_INVALIDARG;
  }
  *pdwStatus = 0;
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::Show(BOOL fShow) {
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::GetTooltipString(BSTR *pbstrToolTip) {
  if (!pbstrToolTip) {
    return E_INVALIDARG;
  }
  *pbstrToolTip = SysAllocString(L"Input mode");
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::OnClick(
    TfLBIClick click, POINT pt, const RECT *prcArea) {
  handlers_->ToggleInputMode();
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::InitMenu(ITfMenu *menu) {
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::OnMenuSelect(UINT wID) {
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::GetIcon(HICON *phIcon) {
  if (!phIcon) {
    return E_INVALIDARG;
  }
  // Use a built-in icon for a while...
  if (state_->input_mode() == InputMode::kDirect) {
    *phIcon = LoadIcon(NULL, IDI_APPLICATION);
  } else {
    *phIcon = LoadIcon(NULL, IDI_ASTERISK);
  }
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::GetText(BSTR *pbstrText) {
  if (!pbstrText) {
    return E_INVALIDARG;
  }
  *pbstrText = SysAllocString(kItemDescription);
  return (*pbstrText ? S_OK : E_OUTOFMEMORY);
}

HRESULT __stdcall InputModeToggleButton::AdviseSink(REFIID riid, IUnknown *punk, DWORD *pdwCookie) {
  if (!IsEqualIID(IID_ITfLangBarItemSink, riid)) {
    return CONNECT_E_CANNOTCONNECT;
  }

  // Support only one sink once.                                                                                                                           
  if (lang_bar_item_sink_ != nullptr) {
    return CONNECT_E_ADVISELIMIT;
  }

  if (punk == nullptr) {
    return E_INVALIDARG;
  }
  if (punk->QueryInterface(IID_ITfLangBarItemSink, (void **)&lang_bar_item_sink_) != S_OK) {
    lang_bar_item_sink_ = nullptr;
    return E_NOINTERFACE;
  }

  *pdwCookie = kCookie;
  return S_OK;
}

HRESULT __stdcall InputModeToggleButton::UnadviseSink(DWORD dwCookie) {
  // Check the given cookie.                                                                                                                                  
  if (dwCookie != kCookie) {
    return CONNECT_E_NOCONNECTION;
  }

  // If there is no connected sink, we just fail.                                                                                                            
  if (lang_bar_item_sink_ == nullptr) {
    return CONNECT_E_NOCONNECTION;
  }

  lang_bar_item_sink_->Release();
  lang_bar_item_sink_ = nullptr;
  return S_OK;
}

ITfLangBarItemSink *InputModeToggleButton::item_sink() {
  return lang_bar_item_sink_;
}


} // langbar
} // text_service
} // senn_win
} // senn
