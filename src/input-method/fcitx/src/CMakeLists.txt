
include_directories(${FCITX4_FCITX_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_CONFIG_INCLUDE_DIRS})
include_directories(${FCITX4_FCITX_UTILS_INCLUDE_DIRS})

set(FCITX_HACHEE_SRC client.cpp fcitx.cpp hachee.cpp ipc.cpp ui.cpp)

## A macro for easy building of the shared object library.
fcitx_add_addon_full(hachee
  SOURCES ${FCITX_HACHEE_SRC}
  IM_CONFIG hachee.conf)
