// tip.cpp : DLL アプリケーション用にエクスポートされる関数を定義します。
//

#include "stdafx.h"

#include "senn.h"
#include "variable.h"

using namespace senn;


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
