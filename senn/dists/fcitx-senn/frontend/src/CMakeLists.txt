
include_directories(${FCITX4_FCITX_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_CONFIG_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_UTILS_INCLUDE_DIRS})

include_directories(../../../../third-party/)
include_directories(../../../../src/cpp/)

set(FCITX_SENN_SRC
  fcitx_senn.cpp
  ../../../../src/cpp/ipc/ipc.cpp
  ../../../../src/cpp/ipc/request.cpp
  ../../../../src/cpp/senn_fcitx/stateful_im_proxy_ipc.cpp
  ../../../../src/cpp/senn_fcitx/stateful_im_proxy_ipc_server.cpp
  ../../../../src/cpp/senn_fcitx/ui.cpp)

## A macro for easy building of the shared object library.
fcitx_add_addon_full(senn
  SOURCES ${FCITX_SENN_SRC}
  IM_CONFIG senn.conf)
