#pragma once

#include <msctf.h>
#include <initguid.h>

namespace senn {
namespace win {
namespace settings {

  
// {2EA7F750-3E6B-4F3E-A1D9-8F28E607217D}
static const GUID kClsid =
{ 0x2ea7f750, 0x3e6b, 0x4f3e, { 0xa1, 0xd9, 0x8f, 0x28, 0xe6, 0x7, 0x21, 0x7d } };

static const WCHAR kDescription[] = L"Senn";

static const WCHAR kThreadingModel[] = L"Apartment";


// {3FA24FB1-1DD6-4E90-A81B-0E560D8AF0B4}
static const GUID kProfileGuid =
{ 0x3fa24fb1, 0x1dd6, 0x4e90, { 0xa8, 0x1b, 0xe, 0x56, 0xd, 0x8a, 0xf0, 0xb4 } };

static const WCHAR kProfileDescription[] = L"Senn Text Service";

static const GUID kCategories[] = { GUID_TFCAT_TIP_KEYBOARD };


}
}
}
