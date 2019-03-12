// tip.cpp : DLL アプリケーション用にエクスポートされる関数を定義します。
//

#include "stdafx.h"

#include "senn.h"
#include "variable.h"

using namespace senn::win;

STDAPI DllRegisterServer() {
  return DllRegistration::Register(new DllRegistration(), g_module_handle);
}

STDAPI DllUnregisterServer(void) {
  return DllRegistration::Unregister(new DllRegistration());
}