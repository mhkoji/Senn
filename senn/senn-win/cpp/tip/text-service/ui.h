#pragma once

#include <msctf.h>

#include <string>

namespace senn {
namespace senn_win {
namespace text_service {
namespace ui {

class DisplayAttributeInfo : public ITfDisplayAttributeInfo {
public:
  DisplayAttributeInfo(const GUID &guid,
                       const TF_DISPLAYATTRIBUTE &attribute,
                       const std::wstring &description)
      : guid_(guid),
    description_(description) {
    CopyMemory(&initial_attribute_, &attribute, sizeof(initial_attribute_));
    CopyMemory(&current_attribute_, &attribute, sizeof(current_attribute_));
  }

  HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }

    if (IsEqualIID(riid, IID_IUnknown)) {
      *ppvObject = static_cast<IUnknown *>(this);
    } else if (IsEqualIID(riid, IID_ITfDisplayAttributeInfo)) {
      *ppvObject = static_cast<ITfDisplayAttributeInfo *>(this);
    } else {
      *ppvObject = nullptr;
      return E_NOINTERFACE;
    }
    AddRef();
    return S_OK;
  }

  ULONG __stdcall AddRef(void) override {
    return ++ref_count_;
  }

  ULONG __stdcall Release(void) override {
    if (ref_count_ <= 0) {
      return 0;
    }

    const ULONG count = --ref_count_;
    if (count == 0) {
      delete this;
    }
    return count;
  }


  HRESULT __stdcall GetGUID(GUID* guid) override {
    if (guid == nullptr) {
      return E_INVALIDARG;
    }
    *guid = guid_;
    return S_OK;
  }

  HRESULT __stdcall GetDescription(BSTR* description) override {
    if (description == nullptr) {
      return E_INVALIDARG;
    }
    *description = SysAllocString(description_.c_str());
    return (*description != nullptr) ? S_OK : E_OUTOFMEMORY;
  }

  HRESULT __stdcall GetAttributeInfo(TF_DISPLAYATTRIBUTE* attribute) override {
    if (attribute == nullptr) {
      return E_INVALIDARG;
    }
    CopyMemory(
        attribute, &current_attribute_, sizeof(current_attribute_));
    return S_OK;
  }

  HRESULT __stdcall SetAttributeInfo(const TF_DISPLAYATTRIBUTE* attribute) override {
    if (attribute == nullptr) {
      return E_INVALIDARG;
    }
    CopyMemory(
        &current_attribute_, attribute, sizeof(current_attribute_));
    return S_OK;
  }

  HRESULT __stdcall Reset(void) override {
    return SetAttributeInfo(&initial_attribute_);
  }

private:

  GUID guid_;

  std::wstring description_;

  TF_DISPLAYATTRIBUTE current_attribute_;

  TF_DISPLAYATTRIBUTE initial_attribute_;

  ULONG ref_count_ = 1;
};


class EnumDisplayAttributeInfo : public IEnumTfDisplayAttributeInfo {
  HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject) override {
    if (ppvObject == nullptr) {
      return E_INVALIDARG;
    }

    if (IsEqualIID(riid, IID_IUnknown)) {
      *ppvObject = static_cast<IUnknown *>(this);
    } else if (IsEqualIID(riid, IID_IEnumTfDisplayAttributeInfo)) {
      *ppvObject = static_cast<IEnumTfDisplayAttributeInfo *>(this);
    } else {
      *ppvObject = nullptr;
      return E_NOINTERFACE;
    }
    AddRef();
    return S_OK;
  }

  ULONG __stdcall AddRef(void) override {
    return ++ref_count_;
  }

  ULONG __stdcall Release(void) override {
    if (ref_count_ <= 0) {
      return 0;
    }

    const ULONG count = --ref_count_;
    if (count == 0) {
      delete this;
    }
    return count;
  }

  // IEnumTfDisplayAttributeInfo
  HRESULT __stdcall Clone(IEnumTfDisplayAttributeInfo ** ppEnum) override;
  HRESULT __stdcall Next(ULONG ulCount, ITfDisplayAttributeInfo ** rgInfo, ULONG * pcFetched) override;
  HRESULT __stdcall Reset(void) override;
  HRESULT __stdcall Skip(ULONG ulCount) override;

private:

  LONG index_ = 0;

  ULONG ref_count_ = 1;
};


ITfRange *InsertTextAndStartComposition(
    const std::wstring&,
    TfEditCookie,
    ITfContext*,
    ITfCompositionSink*,
    ITfComposition**);

ITfRange *ReplaceTextInComposition(
    const std::wstring&,
    TfEditCookie,
    ITfComposition*);

void SetDisplayAttribute(
    TfEditCookie,
    ITfContext*,
    ITfRange*,
    TfGuidAtom attribute_atom);

namespace editing {

// {BEE1A1BF-30E0-4D26-9F56-B7D7207EB2D5}
static const GUID kDisplayAttributeGuid =
    { 0xbee1a1bf, 0x30e0, 0x4d26, { 0x9f, 0x56, 0xb7, 0xd7, 0x20, 0x7e, 0xb2, 0xd5 } };

static TF_DISPLAYATTRIBUTE kDisplayAttribute = {
    { TF_CT_NONE, 0 },        // text color
    { TF_CT_NONE, 0 },        // background color
    TF_LS_DOT,                // underline style
    FALSE,                    // underline boldness
    { TF_CT_NONE, 0 },        // underline color
    TF_ATTR_INPUT             // attribute info
};

class DisplayAttributeInfo
      : public senn::senn_win::text_service::ui::DisplayAttributeInfo {
public:
  DisplayAttributeInfo()
    : senn::senn_win::text_service::ui::DisplayAttributeInfo (
          kDisplayAttributeGuid,
          kDisplayAttribute,
          L"Display Attribute Edit") {}
};

} // editing

} // ui
} // text_service
} // win
} // senn
