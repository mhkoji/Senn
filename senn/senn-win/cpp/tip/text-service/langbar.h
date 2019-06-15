#pragma once


#include <msctf.h>

#include <functional>
#include <string>
#include "input_mode.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace langbar {

  // {AE431567-8E4E-4A58-895D-2E6EE7D25AD1}
static const GUID kItemId =
  { 0xae431567, 0x8e4e, 0x4a58, { 0x89, 0x5d, 0x2e, 0x6e, 0xe7, 0xd2, 0x5a, 0xd1 } };

static const WCHAR kItemDescription[] = L"Input mode menu button";

// We have to implement both the ITfSource and one of the ITfLangBarItem.
// https://docs.microsoft.com/ja-jp/windows/desktop/tsf/language-bar
class InputModeToggleButton : public ITfLangBarItemButton, public ITfSource {
public:
  class State {
  public:
    virtual InputMode input_mode() const = 0;
  };

  class Handlers {
  public:
    virtual void ToggleInputMode() = 0;
  };

  InputModeToggleButton(CLSID, ULONG, State*, Handlers*);

  HRESULT __stdcall QueryInterface(REFIID riid, void** ppvObject) {
    if (ppvObject == NULL) {
      return E_INVALIDARG;
    }
    if (IsEqualIID(riid, IID_IUnknown) ||
        IsEqualIID(riid, IID_ITfLangBarItem) ||
        IsEqualIID(riid, IID_ITfLangBarItemButton)) {
      *ppvObject = static_cast<ITfLangBarItemButton *>(this);
    } else if (IsEqualIID(riid, IID_ITfSource)) {
      *ppvObject = static_cast<ITfSource *>(this);  
    } else {
      *ppvObject = NULL;
      return E_NOINTERFACE;
    }
    AddRef();
    return S_OK;
  }

  ULONG __stdcall AddRef(void) {
    return ++ref_count_;
  }

  ULONG __stdcall Release(void) {
    if (ref_count_ <= 0) {
      return 0;
    }

    const ULONG count = --ref_count_;
    if (count == 0) {
      delete this;
    }
    return count;
  }

  // ITfSource
  virtual HRESULT __stdcall AdviseSink(REFIID riid, IUnknown * punk, DWORD * pdwCookie) override;
  virtual HRESULT __stdcall UnadviseSink(DWORD dwCookie) override;

  // ITfLangBarItemButton
  virtual HRESULT __stdcall GetInfo(TF_LANGBARITEMINFO * pInfo) override;
  virtual HRESULT __stdcall GetStatus(DWORD * pdwStatus) override;
  virtual HRESULT __stdcall Show(BOOL fShow) override;
  virtual HRESULT __stdcall GetTooltipString(BSTR * pbstrToolTip) override;
  virtual HRESULT __stdcall OnClick(TfLBIClick click, POINT pt, const RECT * prcArea) override;
  virtual HRESULT __stdcall InitMenu(ITfMenu * pMenu) override;
  virtual HRESULT __stdcall OnMenuSelect(UINT wID) override;
  virtual HRESULT __stdcall GetIcon(HICON * phIcon) override;
  virtual HRESULT __stdcall GetText(BSTR * pbstrText) override;

  ITfLangBarItemSink *item_sink();

private:
  ULONG ref_count_;

  const CLSID clsid_;

  const ULONG sort_;

  ITfLangBarItemSink *lang_bar_item_sink_;

  const State* state_;

  Handlers* handlers_;

  static const DWORD kCookie = 0;
};

} // langbar
} // text_service
} // senn_win
} // senn
