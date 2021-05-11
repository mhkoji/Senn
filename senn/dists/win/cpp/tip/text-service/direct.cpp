#include "direct.h"

namespace senn {
namespace senn_win {
namespace text_service {
namespace direct {

DirectKeyEventHandler::DirectKeyEventHandler() {}

HRESULT DirectKeyEventHandler::OnSetFocus(BOOL fForeground) { return S_OK; }

HRESULT DirectKeyEventHandler::OnTestKeyDown(ITfContext *context, WPARAM wParam,
                                             LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT DirectKeyEventHandler::OnKeyDown(ITfContext *context, WPARAM wParam,
                                         LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT DirectKeyEventHandler::OnTestKeyUp(ITfContext *context, WPARAM wParam,
                                           LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT DirectKeyEventHandler::OnKeyUp(ITfContext *context, WPARAM wParam,
                                       LPARAM lParam, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

HRESULT DirectKeyEventHandler::OnPreservedKey(ITfContext *context,
                                              REFGUID rguid, BOOL *pfEaten) {
  *pfEaten = false;
  return S_OK;
}

} // namespace direct
} // namespace text_service
} // namespace senn_win
} // namespace senn
