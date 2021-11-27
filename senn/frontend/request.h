#pragma once
#include <string>

namespace senn {

class RequesterInterface {
public:
  virtual ~RequesterInterface(){};

  virtual void Request(const std::string &, std::string *) = 0;
};

} // namespace senn
