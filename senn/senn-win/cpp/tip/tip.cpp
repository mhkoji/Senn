// tip.cpp : DLL アプリケーション用にエクスポートされる関数を定義します。
//

#include "stdafx.h"

#include "senn.h"
#include "variable.h"

using namespace senn;


class DllLocker :
    public senn_win::text_service::TextServiceFactory::ServerLocker {
public:
  void Lock() override;
  void Unlock() override;
};

void DllLocker::Lock() {
  InterlockedIncrement(&g_dll_ref_count);
}

void DllLocker::Unlock() {
  if (0 < InterlockedDecrement(&g_dll_ref_count)) {
    return;
  }

  EnterCriticalSection(&g_CS);

  if (g_class_factory_objects[0] != nullptr) {
    delete g_class_factory_objects[0];
  }

  LeaveCriticalSection(&g_CS);
}


STDAPI DllRegisterServer() {
  return win::text_service::DllRegistration::Register(
      new win::text_service::DllRegistration(&senn_win::kClsid),
      new senn_win::registration::COMServerSettingsProvider(g_module_handle),
      new senn_win::registration::TextServiceSettingsProvider());
}

STDAPI DllUnregisterServer(void) {
  return win::text_service::DllRegistration::Unregister(
      new win::text_service::DllRegistration(&senn_win::kClsid),
      new senn_win::registration::TextServiceSettingsProvider());
}

STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, void** ppv) {
  if (g_class_factory_objects[0] == nullptr) {
    EnterCriticalSection(&g_CS);

    // need to check ref again after grabbing mutex                                                                                                         
    if (g_class_factory_objects[0] == nullptr) {
      g_class_factory_objects[0] =
          new senn_win::text_service::TextServiceFactory(new DllLocker());
    }

    LeaveCriticalSection(&g_CS);
  }

  if (IsEqualIID(riid, IID_IClassFactory) || IsEqualIID(riid, IID_IUnknown)) {
    for (size_t i = 0; i < ARRAYSIZE(g_class_factory_objects); i++) {
      if (g_class_factory_objects[i] != nullptr &&
          IsEqualGUID(rclsid, g_class_factory_objects[i]->GetIid())) {
        *ppv = (void *)g_class_factory_objects[i];
        InterlockedIncrement(&g_dll_ref_count);
        return NOERROR;
      }
    }
  }

  *ppv = nullptr;
  return CLASS_E_CLASSNOTAVAILABLE;
}
