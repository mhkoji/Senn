#pragma once

#include "stdafx.h"
#include <windows.h>


namespace senn {
namespace win {
namespace registry {

class COMServerRegisterable {
  public:
    virtual ~COMServerRegisterable() {}

    virtual const BYTE* GetDescription() const = 0;
    virtual DWORD GetDescriptionBytes()  const = 0;

    virtual const BYTE* GetThreadingModel() const = 0;
    virtual DWORD GetThreadingModelBytes()  const = 0;


    static BOOL Register(
        const COMServerRegisterable* const,
        const GUID&,
        HINSTANCE);
  };

} // registry
} // win
} // senn
