#pragma once

namespace senn {
namespace senn_win {
namespace text_service {

template <typename T> class ObjectReleaser {
public:
  ObjectReleaser(T *obj) : obj_(obj) {}

  ~ObjectReleaser() {
    if (obj_) {
      obj_->Release();
    }
  }

private:
  T *obj_;
};

} // namespace text_service
} // namespace senn_win
} // namespace senn
