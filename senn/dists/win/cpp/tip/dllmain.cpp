// dllmain.cpp : DLL アプリケーションのエントリ ポイントを定義します。
#include "stdafx.h"
#include "variable.h"
#include "senn.h"
#include "text-service/hiragana/candidate_window.h"


BOOL APIENTRY DllMain(HMODULE hModule,
                      DWORD  ul_reason_for_call,
                      LPVOID lpReserved) {
    switch (ul_reason_for_call) {
    case DLL_PROCESS_ATTACH:
      g_module_handle = hModule;
      if (!InitializeCriticalSectionAndSpinCount(&g_CS, 0)) {
        return FALSE;
      }
      senn::senn_win::text_service::hiragana::CandidateWindow::RegisterWindowClass();
      break;
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
    case DLL_PROCESS_DETACH:
      DeleteCriticalSection(&g_CS);
      senn::senn_win::text_service::hiragana::CandidateWindow::UnregisterWindowClass();
      break;
    }
    return TRUE;
}
