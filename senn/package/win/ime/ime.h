#pragma once
#include <string>

#ifdef IME_EXPORTS
#define IME_API __declspec(dllexport)
#else
#define IME_API __declspec(dllimport)
#endif

class IME_API IME {
public:
  IME();

  ~IME();

  void HandleRequest(const std::string &, std::string *);

private:
  class Impl;
  Impl *impl_;
};
