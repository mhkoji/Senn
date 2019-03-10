// tip.cpp : DLL アプリケーション用にエクスポートされる関数を定義します。
//

#include "stdafx.h"

#include "senn.h"
#include "variable.h"

using namespace senn::win;

STDAPI DllRegisterServer() {
  return SennRegistration::Register(new SennRegistration(), g_module_handle);
}
