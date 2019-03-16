#pragma once

namespace senn {
namespace senn_win {
namespace text_service {

template <typename T>
class ObjectReleaser {
public:
  ObjectReleaser(T *pobj) : pobj(pobj) {}

  ~ObjectReleaser() {
    if (pobj) {
      pobj->Release();
    }
  }

private:
  T *pobj;
};

}  // text_service
}  // senn_win
}  // senn
