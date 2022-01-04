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
set(FCITX_SENN_INSTALL_DIR /usr/lib/senn/fcitx)

## Without this, CMake sets runtime path of the shared object to "".
set(CMAKE_INSTALL_RPATH ${FCITX_SENN_INSTALL_DIR})

include_directories(
  ${FCITX4_FCITX_INCLUDE_DIRS}
  ${FCITX4_FCITX_CONFIG_INCLUDE_DIRS}
  ${FCITX4_FCITX_UTILS_INCLUDE_DIRS}
  ../../third-party/
  ${SENN_CPP_DIR}
#  $ENV{HOME}/.roswell/impls/x86-64/linux/ecl/21.2.1/include
  )

## A macro to build the shared object library easily.
fcitx_add_addon_full(senn
  SOURCES fcitx_senn.cpp
          ${SENN_CPP_DIR}/process/process.cpp
          ${SENN_CPP_DIR}/fcitx/im/stateful_ime_proxy.cpp
          ${SENN_CPP_DIR}/fcitx/im/stateful_ime_ecl.cpp
  LINK_LIBS ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx--all-systems.a
            ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/libecl.so
  IM_CONFIG senn.conf)

## This fcitx-senn.png is displayed in the tray as the senn icon.
install(
  FILES ../../icon/senn.png
  DESTINATION share/icons/hicolor/48x48/apps
  RENAME fcitx-senn.png)

install(
  FILES ../../icon/senn.png
  DESTINATION share/fcitx/imicon)

install(
  ## CMake adds this file to the dependencies of the shared object.
  PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/libecl.so.21.2
  DESTINATION ${FCITX_SENN_INSTALL_DIR})

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/ecl-21.2.1
  DESTINATION ${FCITX_SENN_INSTALL_DIR})

install(
  PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc/kkc-engine
  DESTINATION ${FCITX_SENN_INSTALL_DIR})

# install(
#   PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/dep-menu/menu-about
#   RENAME menu-about
#   CONFIGURATIONS Release
#   RUNTIME DESTINATION ${FCITX_SENN_INSTALL_DIR})

