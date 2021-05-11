#pragma once

#include <msctf.h>

#include <string>

namespace senn {
namespace senn_win {
namespace text_service {
namespace direct {

class DirectKeyEventHandler {
public:
  DirectKeyEventHandler();

  HRESULT OnSetFocus(BOOL fForeground);
  HRESULT OnTestKeyDown(ITfContext *pic, WPARAM wParam, LPARAM lParam,
                        BOOL *pfEaten);
  HRESULT OnTestKeyUp(ITfContext *pic, WPARAM wParam, LPARAM lParam,
                      BOOL *pfEaten);
  HRESULT OnKeyDown(ITfContext *pic, WPARAM wParam, LPARAM lParam,
                    BOOL *pfEaten);
  HRESULT OnKeyUp(ITfContext *pic, WPARAM wParam, LPARAM lParam, BOOL *pfEaten);
  HRESULT OnPreservedKey(ITfContext *pic, REFGUID rguid, BOOL *pfEaten);

private:
};

} // namespace direct
} // namespace text_service
} // namespace senn_win
} // namespace senn
