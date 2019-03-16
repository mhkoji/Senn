#pragma once

#include <msctf.h>

#include "registry.h"
#include "text_service_registration.h"
#include "text_service_class_factory.h"

namespace senn {
namespace senn_win {

  // {2EA7F750-3E6B-4F3E-A1D9-8F28E607217D}
static const GUID kClsid =
  { 0x2ea7f750, 0x3e6b, 0x4f3e, { 0xa1, 0xd9, 0x8f, 0x28, 0xe6, 0x7, 0x21, 0x7d } };


static const WCHAR kDescription[] = L"Senn";

static const WCHAR kThreadingModel[] = L"Apartment";


// {3FA24FB1-1DD6-4E90-A81B-0E560D8AF0B4}
static const GUID kProfileGuid =
  { 0x3fa24fb1, 0x1dd6, 0x4e90, { 0xa8, 0x1b, 0xe, 0x56, 0xd, 0x8a, 0xf0, 0xb4 } };

static const WCHAR kProfileDescription[] = L"Senn Text Service";

static const GUID kCategories[] = {
  GUID_TFCAT_TIP_KEYBOARD,
  // The text service implments ITfDisplayAttributeProvider
  // in order to decorate composing texts using display attribute utilities.
  GUID_TFCAT_DISPLAYATTRIBUTEPROVIDER,
};


static const WCHAR kNamedPipePath[] = L"\\\\.\\pipe\\senn";


namespace registration { /////////////////////////////////////////////////////////

class COMServerSettingsProvider
  : public ::senn::win::registry::com_server::SettingsProvider {
public:
  COMServerSettingsProvider(HINSTANCE module_handle) :
      module_handle_(module_handle) {
  }

  BOOL Get(::senn::win::registry::com_server::Settings *output) const override {
    DWORD size = ARRAYSIZE(output->module_file_name.content);
    DWORD num_chars = GetModuleFileName(
        module_handle_, output->module_file_name.content, size);
    if (num_chars == 0) {
      return FALSE;
    }
    output->module_file_name.size_including_null_termination =
      num_chars < (size - 1) ? num_chars + 1 : size;

    output->description.content = (const BYTE*)kDescription;
    output->description.bytes = (_countof(kDescription)) * sizeof(WCHAR);

    output->threading_model.content = (const BYTE*)kThreadingModel;
    output->threading_model.bytes = (_countof(kThreadingModel)) * sizeof(WCHAR);

    return TRUE;
  }

private:
  const HINSTANCE module_handle_;
};

class TextServiceSettingsProvider
  : public ::senn::win::text_service::registration::SettingsProvider {
public:
  void Get(::senn::win::text_service::registration::Settings *output) const override {
    output->profile_guid = kProfileGuid;
    output->profile_description = kProfileDescription;

    size_t category_count =
        static_cast<size_t>(sizeof(kCategories) / sizeof(kCategories[0]));
    for (size_t i = 0; i < category_count; i++) {
      output->categories.push_back(kCategories[i]);
    }
  }
};

} // registration /////////////////////////////////////////////////////////


namespace text_service { /////////////////////////////////////////////////////////

namespace ui { /////////////////////////////////////////////////////////

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
} // ui /////////////////////////////////////////////////////////


class EditSession : public ITfEditSession {
public:
  EditSession(ITfCompositionSink*,
              ITfContext*,
              const std::wstring&,
              ITfComposition**,
              TfGuidAtom);
  ~EditSession();

  // IUnknow
  HRESULT __stdcall QueryInterface(REFIID riid, void **ppvObject) {
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

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec);

  ITfCompositionSink *composition_sink_;
  
  ITfContext* const context_;

  const std::wstring& text_;

  ITfComposition** const composition_;

  const TfGuidAtom editing_atom_;

  ULONG ref_count_ = 1;
};


class TextService
    : public ITfKeyEventSink,
      public ITfDisplayAttributeProvider,
      public ITfCompositionSink,
      public ITfTextInputProcessor {
public:
  // IUnknow
  HRESULT __stdcall QueryInterface(REFIID riid, void** ppvObject) {
    if (ppvObject == NULL) {
      return E_INVALIDARG;
    }
    if (IsEqualIID(riid, IID_IUnknown) ||
        IsEqualIID(riid, IID_ITfTextInputProcessor)) {
      *ppvObject = static_cast<ITfTextInputProcessor *>(this);
    } else if (IsEqualIID(riid, IID_ITfKeyEventSink)) {
      *ppvObject = static_cast<ITfKeyEventSink *>(this);
    } else if (IsEqualIID(riid, IID_ITfDisplayAttributeProvider)) {
      *ppvObject = static_cast<ITfDisplayAttributeProvider *>(this);
    } else if (IsEqualIID(riid, IID_ITfDisplayAttributeProvider)) {
      *ppvObject = static_cast<ITfCompositionSink *>(this);
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

  // ITfKeyEventSink
  HRESULT __stdcall OnSetFocus(BOOL fForeground) override;
  HRESULT __stdcall OnTestKeyDown(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnTestKeyUp(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnKeyDown(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnKeyUp(ITfContext*, WPARAM, LPARAM, BOOL*) override;
  HRESULT __stdcall OnPreservedKey(ITfContext*, REFGUID, BOOL*) override;

  // ITfTextInputProcessor
  HRESULT Activate(ITfThreadMgr*, TfClientId);
  HRESULT Deactivate();

  // ITfCompositionSink
  HRESULT __stdcall OnCompositionTerminated(TfEditCookie, ITfComposition*) override;

  // ITfDisplayAttributeProvider
  HRESULT __stdcall EnumDisplayAttributeInfo(IEnumTfDisplayAttributeInfo**) override;
  HRESULT __stdcall GetDisplayAttributeInfo(REFGUID, ITfDisplayAttributeInfo**) override;

private:

  ITfThreadMgr *thread_mgr_;

  TfClientId client_id_;


  ITfContext *context_ = nullptr;

  std::wstring text_ = L"";

  ITfComposition *composition_;

  TfGuidAtom editing_atom_;


  struct {
    TfGuidAtom editing_atom = TF_INVALID_GUIDATOM;
  } display_attribute_vals_;


  ULONG ref_count_ = 1;
};

class TextServiceFactory
    : public senn::win::text_service::ClassFactory<TextService> {
public:
  class ServerLocker {
  public:
    virtual ~ServerLocker() {};

    virtual void Lock() = 0;

    virtual void Unlock() = 0;
  };

  TextServiceFactory(ServerLocker* const locker) : locker_(locker) {}

  REFIID GetIid() {
    return kClsid;
  }
 
  HRESULT __stdcall LockServer(BOOL lock) override {
    if (lock) {
      locker_->Lock();
    } else {
      locker_->Unlock();
    }
    return S_OK;
  }

private:

  ServerLocker* const locker_;
};

} // text_service /////////////////////////////////////////////////////////


} // win
} // senn
