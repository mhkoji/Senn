#include "stdafx.h"
#include "senn.h"

HINSTANCE g_module_handle;

CRITICAL_SECTION g_CS;

senn::senn_win::text_service::TextServiceFactory* g_class_factory_objects[1] =
    { nullptr };

LONG g_dll_ref_count = 0;
