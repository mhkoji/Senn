#include <atlbase.h>
#include <strsafe.h>
#include "langbar.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace langbar {

InputModeMenuButton::InputModeMenuButton(CLSID clsid, ULONG sort)
  : ref_count_(1), clsid_(clsid), sort_(sort), lang_bar_item_sink_(nullptr) {
}

HRESULT __stdcall InputModeMenuButton::GetInfo(TF_LANGBARITEMINFO *pInfo) {
  if (!pInfo) {
    return E_INVALIDARG;
  }
  pInfo->clsidService = clsid_;
  pInfo->guidItem = langbar::kItemId;
  // TF_LBI_STYLE_SHOWNINTRAY seems to enable the item shown in the taskbar.
  // (But, this doesn't work...)
  pInfo->dwStyle = TF_LBI_STYLE_BTN_MENU | TF_LBI_STYLE_SHOWNINTRAY;
  pInfo->ulSort = sort_;
  StringCchCopy(pInfo->szDescription,
                ARRAYSIZE(pInfo->szDescription),
                kItemDescription);
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::GetStatus(DWORD *pdwStatus) {
  if (!pdwStatus) {
    return E_INVALIDARG;
  }
  *pdwStatus = 0;
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::Show(BOOL fShow) {
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::GetTooltipString(BSTR *pbstrToolTip) {
  if (!pbstrToolTip) {
    return E_INVALIDARG;
  }
  *pbstrToolTip = SysAllocString(L"Input mode");
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::OnClick(
    TfLBIClick click, POINT pt, const RECT *prcArea) {
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::InitMenu(ITfMenu *menu) {
  const WCHAR *item_names[] = {
     L"Direct input",
     L"Latin-hiragana input"
  };
  for (size_t i = 0; i < ARRAYSIZE(item_names); i++) {
    const WCHAR* item_name = item_names[i];
    size_t item_name_len = wcslen(item_name);
    menu->AddMenuItem(i,
                      i == 0 ? TF_LBMENUF_CHECKED : 0,
                      NULL, NULL,
                      item_name, item_name_len, NULL);
  }
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::OnMenuSelect(UINT wID) {
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::GetIcon(HICON *phIcon) {
  if (!phIcon) {
    return E_INVALIDARG;
  }
  // Use a built-in icon for a while...
  *phIcon = LoadIcon(NULL, IDI_APPLICATION);
  return S_OK;
}

HRESULT __stdcall InputModeMenuButton::GetText(BSTR *pbstrText) {
  if (!pbstrText) {
    return E_INVALIDARG;
  }
  *pbstrText = SysAllocString(kItemDescription);
  return (*pbstrText ? S_OK : E_OUTOFMEMORY);
}

HRESULT __stdcall InputModeMenuButton::AdviseSink(REFIID riid, IUnknown *punk, DWORD *pdwCookie) {
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

HRESULT __stdcall InputModeMenuButton::UnadviseSink(DWORD dwCookie) {
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


} // langbar
} // text_service
} // senn_win
} // senn
