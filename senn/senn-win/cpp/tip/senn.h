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
  GUID_TFCAT_TIP_KEYBOARD
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

class EditSession :
  public ITfEditSession {
public:
  EditSession(ITfContext *);
  ~EditSession();

  // IUnknow
  HRESULT __stdcall QueryInterface(REFIID riid, void ** ppvObject) override;
  ULONG __stdcall AddRef(void) override;
  ULONG __stdcall Release(void) override;

private:
  // ITfEditSession
  HRESULT __stdcall DoEditSession(TfEditCookie ec) override;

  ITfContext *context_;

  ULONG ref_count_ = 0;


  static void* CastSelf(EditSession*, REFIID);
};


class TextService
    : public ITfKeyEventSink,
      public ITfTextInputProcessor {
public:
  // IUnknow
  HRESULT __stdcall QueryInterface(REFIID riid, void ** ppvObject) override;
  ULONG __stdcall AddRef(void) override;
  ULONG __stdcall Release(void) override;

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

private:

  ITfThreadMgr *thread_mgr_;

  TfClientId client_id_;

  ULONG ref_count_ = 0;


  static void* CastSelf(TextService*, REFIID);
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
