#pragma once

#include <combaseapi.h>
#include <msctf.h>

namespace senn {
namespace win {
namespace text_service {

template <typename T> class ClassFactory : public IClassFactory {
public:
  HRESULT __stdcall QueryInterface(REFIID riid, void **ppv) override {
    if (ppv == nullptr) {
      return E_INVALIDARG;
    }
    if (IsEqualIID(riid, IID_IUnknown)) {
      *ppv = static_cast<IUnknown *>(this);
    } else if (::IsEqualIID(riid, IID_IClassFactory)) {
      *ppv = static_cast<IClassFactory *>(this);
    } else {
      *ppv = nullptr;
      return E_NOINTERFACE;
    }
    AddRef();
    return S_OK;
  }

  HRESULT __stdcall CreateInstance(IUnknown *unk, REFIID riid,
                                   void **ppv) override {
    if (ppv == nullptr) {
      return E_INVALIDARG;
    }

    if (unk != nullptr) {
      return CLASS_E_NOAGGREGATION;
    }

    *ppv = nullptr;
    T *obj = new T();
    if (obj == nullptr) {
      return E_OUTOFMEMORY;
    }

    HRESULT result = obj->QueryInterface(riid, ppv);

    obj->Release();

    return result;
  }

  ULONG __stdcall AddRef(void) override { return ++ref_count_; }

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

  // IClassFactory
  virtual HRESULT __stdcall LockServer(BOOL) = 0;

private:
  ULONG ref_count_ = 0;
};

} // namespace text_service
} // namespace win
} // namespace senn
