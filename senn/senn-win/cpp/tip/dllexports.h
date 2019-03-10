#pragma once

#include <msctf.h>
#include <initguid.h>


// {2EA7F750-3E6B-4F3E-A1D9-8F28E607217D}
static const GUID TEXT_SERVICE_CLSID =
{ 0x2ea7f750, 0x3e6b, 0x4f3e, { 0xa1, 0xd9, 0x8f, 0x28, 0xe6, 0x7, 0x21, 0x7d } };

static const WCHAR TEXT_SERVICE_DESCRIPTION[] = L"Senn";

static const WCHAR TEXT_SERVICE_THREADING_MODEL[] = L"Apartment";


// {3FA24FB1-1DD6-4E90-A81B-0E560D8AF0B4}
static const GUID TEXT_SERVICE_PROFILE_GUID =
{ 0x3fa24fb1, 0x1dd6, 0x4e90, { 0xa8, 0x1b, 0xe, 0x56, 0xd, 0x8a, 0xf0, 0xb4 } };

static const WCHAR TEXT_SERVICE_PROFILE_DESCRIPTION[] = L"Senn Text Service";

static const GUID TEXT_SERVICE_CATEGORIES[] = { GUID_TFCAT_TIP_KEYBOARD };
