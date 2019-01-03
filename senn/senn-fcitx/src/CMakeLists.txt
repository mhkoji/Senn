
include_directories(${FCITX4_FCITX_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_CONFIG_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_UTILS_INCLUDE_DIRS})

set(FCITX_SENN_SRC client.cpp fcitx.cpp senn.cpp ipc.cpp ui.cpp)

## A macro for easy building of the shared object library.
fcitx_add_addon_full(senn
  SOURCES ${FCITX_SENN_SRC}
  IM_CONFIG senn.conf)
