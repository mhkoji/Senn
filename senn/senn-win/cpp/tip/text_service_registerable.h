#pragma once

#include <windows.h>
#include <vector>


namespace senn {
namespace win {
namespace text_service {

class TextServiceRegisterable;
class TextServiceRegisterable {
public:
  virtual ~TextServiceRegisterable() {}

  virtual const GUID& GetProfileGuid()  const = 0;
  virtual const WCHAR* GetProfileDescription() const = 0;

  virtual std::vector<GUID> GetCategories() const = 0;


  static BOOL Register(
      const TextServiceRegisterable* const,
      const GUID&);
};


} // text_service
} // win
} // senn