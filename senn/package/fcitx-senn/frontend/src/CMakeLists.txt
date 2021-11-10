include_directories(${FCITX4_FCITX_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_CONFIG_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_UTILS_INCLUDE_DIRS})

include_directories(../../../../third-party/)
include_directories(../../../../frontend/)

set(FCITX_SENN_SRC
  fcitx_senn.cpp
  ../../../../frontend/ipc/ipc.cpp
  ../../../../frontend/ipc/request.cpp
  ../../../../frontend/process/process.cpp
  ../../../../frontend/senn_fcitx/im/stateful_ime_proxy_ipc.cpp
  ../../../../frontend/senn_fcitx/im/stateful_ime_proxy_ipc_server.cpp
  )

## A macro for easy building of the shared object library.
fcitx_add_addon_full(senn
  SOURCES ${FCITX_SENN_SRC}
  IM_CONFIG senn.conf)
