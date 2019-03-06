#pragma once

#include "stdafx.h"

BOOL RegisterServer(
    const GUID &clsid,
    const BYTE* description,
    const DWORD description_bytes,
    const BYTE* threading_model,
    const DWORD threading_model_bytes,
    HINSTANCE module_handle);
