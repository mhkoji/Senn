// tip.cpp : DLL アプリケーション用にエクスポートされる関数を定義します。
//

#include "stdafx.h"

#include "senn.h"
#include "variable.h"

using senn::win::g_senn_text_service;

STDAPI DllRegisterServer() {
  if (!g_senn_text_service->RegisterCOMServer(
           g_senn_text_service->GetClsid(),
           g_module_handle)) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  if (!g_senn_text_service->RegisterTextService(
           g_senn_text_service->GetClsid())) {
    // DllUnregisterServer();
    return E_FAIL;
  }

  return S_OK;
}