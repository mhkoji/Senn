#pragma once

#include "stdafx.h"
#include <windows.h>


namespace senn {
namespace win {
namespace registry {


BOOL RegisterCOMServer(
    const GUID &clsid,
    const BYTE* description,
    const DWORD description_bytes,
    const BYTE* threading_model,
    const DWORD threading_model_bytes,
    HINSTANCE module_handle
);


} // registry
} // win
} // senn
