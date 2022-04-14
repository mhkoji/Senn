cmake_minimum_required(VERSION 2.8.6)
project(fcitx-senn)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

find_package(Fcitx 4.2.8 REQUIRED)
include(FindPkgConfig)

set(CMAKE_C_FLAGS "-Wall -Wextra -Wno-sign-compare -Wno-unused-parameter")
set(CMAKE_CXX_FLAGS "-std=c++11 -Wall -Wextra -Wno-sign-compare -Wno-unused-parameter")
set(CMAKE_SHARED_LINKER_FLAGS "-Wl,--as-needed ${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "-Wl,--as-needed ${CMAKE_MODULE_LINKER_FLAGS}")

set(SENN_CPP_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../src-cpp)
set(SENN_INSTALL_DIR /usr/lib/senn)

## Without this, CMake sets runtime path of the shared object to "".
set(CMAKE_INSTALL_RPATH ${SENN_INSTALL_DIR}/fcitx/ecl/lib/)

include_directories(
  ${FCITX4_FCITX_INCLUDE_DIRS}
  ${FCITX4_FCITX_CONFIG_INCLUDE_DIRS}
  ${FCITX4_FCITX_UTILS_INCLUDE_DIRS}
  ../../third-party/
  ${SENN_CPP_DIR}
  )

## fcitx-senn.so
fcitx_add_addon_full(senn
  SOURCES fcitx_senn.cpp
          ${SENN_CPP_DIR}/fcitx/im/stateful_ime_proxy.cpp
          ${SENN_CPP_DIR}/fcitx/im/stateful_ime_ecl.cpp
  LINK_LIBS /usr/lib/senn/fcitx/ecl/lib/libecl.so
            ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx--all-systems.a
  IM_CONFIG senn.conf)

## libuserdict.so
add_library(userdict SHARED
  ${SENN_CPP_DIR}/user-dict/user_dict.cpp
)

### Configs for installation

## This fcitx-senn.png is displayed in the tray as the senn icon.
install(
  FILES ../../icon/senn.png
  DESTINATION share/icons/hicolor/48x48/apps
  RENAME fcitx-senn.png)

install(
  FILES ../../icon/senn.png
  DESTINATION share/fcitx/imicon)

install(
  PROGRAMS /usr/lib/senn/fcitx/ecl/lib/libecl.so.21.2.1
  DESTINATION ${SENN_INSTALL_DIR}/fcitx/ecl/lib/
  ## CMake adds this file to the dependencies of the shared object.
  RENAME libecl.so.21.2)

install(
  DIRECTORY /usr/lib/senn/fcitx/ecl/lib/ecl-21.2.1
  DESTINATION ${SENN_INSTALL_DIR}/fcitx/ecl/lib/)

install(
  # https://bugs.launchpad.net/sbcl/+bug/310108
  # FILES is used instead of PROGRAMS because install with PROGRAMS strips the core of kkc-engine built by sbcl.
  # As a result, kkc-engine is not executable, so make it executable by postinst.
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc/kkc-engine
  DESTINATION ${SENN_INSTALL_DIR}/fcitx)

install(
  TARGETS userdict
  LIBRARY DESTINATION ${SENN_INSTALL_DIR}/)

# install(
#   PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/dep-menu/menu-about
#   RENAME menu-about
#   CONFIGURATIONS Release
#   RUNTIME DESTINATION ${SENN_INSTALL_DIR}/fcitx)

