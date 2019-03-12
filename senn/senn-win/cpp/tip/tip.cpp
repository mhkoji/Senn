// tip.cpp : DLL アプリケーション用にエクスポートされる関数を定義します。
//

#include "stdafx.h"

#include "senn.h"
#include "variable.h"

using namespace senn::win;


STDAPI DllRegisterServer() {
  return text_service::DllRegistration::Register(
      new text_service::DllRegistration(&kClsid),
      new registration::COMServerSettingsProvider(g_module_handle),
      new registration::TextServiceRegistrationSettingsProvider());
}

STDAPI DllUnregisterServer(void) {
  return text_service::DllRegistration::Unregister(
      new text_service::DllRegistration(&kClsid),
      new registration::TextServiceRegistrationSettingsProvider());
}
