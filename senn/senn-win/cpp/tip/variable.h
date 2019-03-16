#pragma once

#include "text-service/text_service.h"

extern HINSTANCE g_module_handle;

extern CRITICAL_SECTION g_CS;

extern senn::senn_win::text_service::TextServiceFactory* g_class_factory_objects[1];

extern LONG g_dll_ref_count;
