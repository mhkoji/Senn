cmake_minimum_required(VERSION 2.8.6)
project(fcitx-senn)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

find_package(Fcitx 4.2.8 REQUIRED)
include(FindPkgConfig)

set(CMAKE_C_FLAGS "-Wall -Wextra -Wno-sign-compare -Wno-unused-parameter")
set(CMAKE_CXX_FLAGS "-Wall -Wextra -Wno-sign-compare -Wno-unused-parameter")
set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--as-needed ${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "-Wl,--as-needed ${CMAKE_MODULE_LINKER_FLAGS}")
## Without this, CMake sets runtime path of "/usr/lib/x86_64-linux-gnu/fcitx/fcitx-senn.so" to ""
set(CMAKE_INSTALL_RPATH "/usr/lib/senn")

include_directories(
  ${FCITX4_FCITX_INCLUDE_DIRS}
  ${FCITX4_FCITX_CONFIG_INCLUDE_DIRS}
  ${FCITX4_FCITX_UTILS_INCLUDE_DIRS}
  ../../../third-party/
  ../../../frontend/
  )

## A macro for easy building of the shared object library.
fcitx_add_addon_full(senn
  SOURCES fcitx_senn.cpp
          ../../../frontend/ipc/ipc.cpp
          ../../../frontend/process/process.cpp
          ../../../frontend/senn_fcitx/im/stateful_ime_proxy.cpp
          ../../../frontend/senn_fcitx/im/stateful_ime_ipc.cpp
  # LINK_LIBS ${CMAKE_CURRENT_SOURCE_DIR}/senn-bin-fcitx-lib--all-systems.a
  #           /usr/lib/senn/libecl.so
  IM_CONFIG senn.conf)

install(
  PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/../backend/bin/server
  CONFIGURATIONS Release
  RUNTIME DESTINATION "/usr/lib/senn")

install(
  PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/../../../frontend/cl/bin/menu-about
  CONFIGURATIONS Release
  RUNTIME DESTINATION "/usr/lib/senn")

## configuration of CPack for generating deb
set(CPACK_GENERATOR "DEB")
set(CPACK_PACKAGE_VERSION_MAJOR "0")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "1")
set(CPACK_DEBIAN_PACKAGE_MAINTAINER "mkoji")

include(CPack)
