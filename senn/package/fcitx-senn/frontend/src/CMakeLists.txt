
include_directories(${FCITX4_FCITX_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_CONFIG_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_UTILS_INCLUDE_DIRS})

include_directories(../../../../third-party/)
include_directories(../../../../frontend/cpp/)

set(FCITX_SENN_SRC
  fcitx_senn.cpp
  ../../../../frontend/cpp/ipc/ipc.cpp
  ../../../../frontend/cpp/ipc/request.cpp
  ../../../../frontend/cpp/process/process.cpp
  ../../../../frontend/cpp/senn_fcitx/stateful_ime_proxy_ipc.cpp
  ../../../../frontend/cpp/senn_fcitx/stateful_ime_proxy_ipc_server.cpp
  ../../../../frontend/cpp/senn_fcitx/ui.cpp)

## A macro for easy building of the shared object library.
fcitx_add_addon_full(senn
  SOURCES ${FCITX_SENN_SRC}
  IM_CONFIG senn.conf)
