#pragma once

#include <initguid.h>


// {2EA7F750-3E6B-4F3E-A1D9-8F28E607217D}
static const GUID TEXT_SERVICE_CLSID =
{ 0x2ea7f750, 0x3e6b, 0x4f3e, { 0xa1, 0xd9, 0x8f, 0x28, 0xe6, 0x7, 0x21, 0x7d } };

static const WCHAR TEXT_SERVICE_DESCRIPTION[] = L"Senn";

static const WCHAR TEXT_SERVICE_THREADING_MODEL[] = L"Apartment";

// strlen("{xxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxx}")
static const size_t CLSID_STRLEN = 38;
