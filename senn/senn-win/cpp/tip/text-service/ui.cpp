#include "object_releaser.h"
#include "ui.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace ui {

HRESULT __stdcall EnumDisplayAttributeInfo::Clone(
    IEnumTfDisplayAttributeInfo **ppEnum) {
  if (ppEnum == nullptr) {
    return E_INVALIDARG;
  }

  EnumDisplayAttributeInfo *clone = new EnumDisplayAttributeInfo();
  clone->index_ = index_;
  *ppEnum = clone;
  return S_OK;
}

HRESULT __stdcall EnumDisplayAttributeInfo::Next(
    ULONG ulCount,
    ITfDisplayAttributeInfo **rgInfo,
    ULONG *pcFetched) {
  ULONG items = 0;
  for (; items < ulCount; ++items) {
    ITfDisplayAttributeInfo *attribute = nullptr;
    if (index_ == 0) {
      attribute = new editing::DisplayAttributeInfo();
    } else {
      break;
    }
    rgInfo[items] = attribute;
    attribute->AddRef();
    ++index_;
  }

  if (pcFetched) {
    *pcFetched = items;
  }

  return (items == ulCount) ? S_OK : S_FALSE;
}

HRESULT __stdcall EnumDisplayAttributeInfo::Reset(void) {
  index_ = 0;
  return S_OK;
}

HRESULT __stdcall EnumDisplayAttributeInfo::Skip(ULONG ulCount) {
  if (0 < ulCount && index_ == 0) {
    ++index_;
  }
  return S_OK;
}

ITfRange *InsertTextAndStartComposition(
    const std::wstring& text,
    TfEditCookie ec,
    ITfContext *context,
    ITfCompositionSink *composition_sink,
    ITfComposition **output) {
  ITfRange *range;

  ITfInsertAtSelection *insert;
  if (context->QueryInterface(IID_ITfInsertAtSelection, (void**)&insert) !=
      S_OK) {
    return nullptr;
  }
  ObjectReleaser<ITfInsertAtSelection> insert_releaser(insert);

  if (insert->InsertTextAtSelection(
          ec, 0, text.c_str(), static_cast<LONG>(text.size()), &range) !=
      S_OK) {
    return nullptr;
  }

  ITfContextComposition *context_composition;
  if (context->QueryInterface(
          IID_ITfContextComposition, (void**)&context_composition) !=
      S_OK) {
    return nullptr;
  }
  ObjectReleaser<ITfContextComposition> context_composition_releader(
      context_composition);

  // MEMO: StartComposition seems to fail if compositin_sink is nullptr.
  if (context_composition->StartComposition(
          ec, range, composition_sink, output) !=
      S_OK) {
    return nullptr;
  }
  return range;
}

ITfRange *ReplaceTextInComposition(
    const std::wstring& text,
    TfEditCookie ec,
    ITfComposition *composition) {
  ITfRange *range;
  composition->GetRange(&range);
  range->SetText(ec, 0, text.c_str(), static_cast<LONG>(text.length()));
  return range;
}


void SetDisplayAttribute(
    TfEditCookie ec,
    ITfContext *context,
    ITfRange *range,
    TfGuidAtom attribute_atom) {
  ITfProperty *display_attribute;
  if (context->GetProperty(GUID_PROP_ATTRIBUTE, &display_attribute) !=
      S_OK) {
    return;
  }
  ObjectReleaser<ITfProperty> releaser(display_attribute);

  VARIANT var;
  var.vt = VT_I4;
  var.lVal = attribute_atom;
  display_attribute->SetValue(ec, range, &var);
}

void RemoveDisplayAttributes(
    TfEditCookie ec,
    ITfContext *context,
    ITfComposition *composition) {
  ITfRange *range;
  if (composition->GetRange(&range) != S_OK) {
    return;
  }
  ObjectReleaser<ITfRange> range_releaser(range);

  ITfProperty *display_attribute;
  if (context->GetProperty(GUID_PROP_ATTRIBUTE, &display_attribute) !=
      S_OK) {
    return;
  }
  ObjectReleaser<ITfProperty> releaser(display_attribute);

  display_attribute->Clear(ec, range);
}


} // ui
} // text_service
} // win
} // senn
