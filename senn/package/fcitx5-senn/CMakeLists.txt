cmake_minimum_required(VERSION 3.6)
project(fcitx5-senn)

find_package(Fcitx5Core 5.0.6 REQUIRED)
# Without GNUInstallDirs, some files like shown below are copied to inappropriate directories:
# - ./fcitx5/inputmethod/senn.conf
# - ./icons/hicolor/32x32/apps/fcitx-senn.png
# - ./metainfo/org.fcitx.Fcitx5.Addon.Senn.metainfo.xml
include(GNUInstallDirs)

set(SENN_ROOT_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../..)
set(SENN_SRC_CPP_DIR ${SENN_ROOT_DIR}/senn/src-cpp)
set(FCITX_SENN_INSTALL_DIR ${CMAKE_INSTALL_LIBDIR}/senn/fcitx)

set(CMAKE_C_FLAGS
  "-Wall -Wextra -Wno-sign-compare -Wno-unused-parameter")
set(CMAKE_CXX_FLAGS
  "-std=c++11 -Wall -Wextra -Wno-sign-compare -Wno-unused-parameter")
set(CMAKE_SHARED_LINKER_FLAGS
  "-Wl,--as-needed ${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS
  "-Wl,--as-needed ${CMAKE_MODULE_LINKER_FLAGS}")

include(
  ${FCITX_INSTALL_CMAKECONFIG_DIR}/Fcitx5Utils/Fcitx5CompilerSettings.cmake)

include_directories(
  ${SENN_SRC_CPP_DIR})

add_library(senn
  MODULE
  fcitx_senn.cpp
  ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_proxy.cpp
  ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_ecl.cpp
  ## This line adds a depencency on the object file to senn.
  senn-lib-fcitx--all-systems.a)
target_link_libraries(senn
  Fcitx5::Core
  Fcitx5::Config
  ecl
  ## This line adds a link to the object file.
  ${CMAKE_CURRENT_BINARY_DIR}/senn-lib-fcitx--all-systems.a)
## The object file name must be senn.so not libsenn.so.
set_target_properties(senn
  PROPERTIES PREFIX "")

add_custom_command(
  OUTPUT senn-lib-fcitx--all-systems.a
  COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/build-lib.sh
  COMMAND mv ${CMAKE_CURRENT_SOURCE_DIR}/senn-lib-fcitx--all-systems.a ${CMAKE_CURRENT_BINARY_DIR})


install(
  FILES senn.conf
  DESTINATION ${CMAKE_INSTALL_DATADIR}/fcitx5/inputmethod)

install(
  FILES senn-addon.conf
  RENAME senn.conf
  DESTINATION ${FCITX_INSTALL_PKGDATADIR}/addon)

install(
  FILES org.fcitx.Fcitx5.Addon.Senn.metainfo.xml
  DESTINATION ${CMAKE_INSTALL_DATADIR}/metainfo)

install(
  TARGETS senn
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/fcitx5)
