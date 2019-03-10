#pragma once

#include "stdafx.h"
#include <vector>

BOOL RegisterCOMServer(
    const GUID &clsid,
    const BYTE* description,
    const DWORD description_bytes,
    const BYTE* threading_model,
    const DWORD threading_model_bytes,
    HINSTANCE module_handle
);


BOOL RegisterTextService(
  const GUID &clsid,
  const GUID &profile_guid,
  const WCHAR *profile_description,
  const ULONG profile_description_len,
  const std::vector<GUID> &categories
);