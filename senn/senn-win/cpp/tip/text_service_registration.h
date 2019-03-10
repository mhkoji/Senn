#pragma once

#include "stdafx.h"
#include <vector>

namespace senn {
namespace win {
namespace text_service_registration {

BOOL Register(
    const GUID &clsid,
    const GUID &profile_guid,
    const WCHAR *profile_description,
    const ULONG profile_description_len,
    const std::vector<GUID> &categories
);

} // text_service_registration
} // win
} // senn
