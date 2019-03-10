// tip.cpp : DLL アプリケーション用にエクスポートされる関数を定義します。
//

#include "stdafx.h"

#include "senn.h"
#include "variable.h"

using senn::win::g_senn_text_service;
using senn::win::registry::COMServerRegisterable;
using senn::win::text_service::TextServiceRegisterable;

STDAPI DllRegisterServer() {
  if (!COMServerRegisterable::Register(
           g_senn_text_service,
           g_senn_text_service->GetClsid(),
           g_module_handle)) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  if (!TextServiceRegisterable::Register(
           g_senn_text_service,
           g_senn_text_service->GetClsid())) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  return S_OK;
}