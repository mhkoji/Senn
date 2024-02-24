cmake_minimum_required(VERSION 3.6)
project(fcitx5-senn)

find_package(Fcitx5Core 5.0.6 REQUIRED)
# Without GNUInstallDirs, some files are copied to inappropriate directories as follows:
# - ./fcitx5/inputmethod/senn.conf
# - ./icons/hicolor/32x32/apps/fcitx-senn.png
# - ./metainfo/org.fcitx.Fcitx5.Addon.Senn.metainfo.xml
include(GNUInstallDirs)

set(SENN_ROOT_DIR         ${CMAKE_CURRENT_SOURCE_DIR}/../../..)
set(SENN_SRC_CPP_DIR      ${SENN_ROOT_DIR}/senn/src-cpp)
set(SENN_THIRD_PARTY_DIR  ${SENN_ROOT_DIR}/third-party/picojson)

set(FCITX_SENN_INSTALL_DIR /usr/lib/senn/fcitx)

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
  ${SENN_SRC_CPP_DIR}
  ${SENN_THIRD_PARTY_DIR}
)

add_library(senn
  MODULE fcitx_senn.cpp
         ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_proxy.cpp
         ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_ecl.cpp)

target_link_libraries(senn
 Fcitx5::Core
 Fcitx5::Config
 ecl
 ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx--all-systems.a
 # ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx-kkc-proxy--all-systems.a
 )

set_target_properties(senn
  PROPERTIES PREFIX "")

#### userdict
add_executable(userdict_main
  ${SENN_ROOT_DIR}/senn-kkc-engine/hachee/src-cpp/user_dict_main.cpp
  ${SENN_ROOT_DIR}/senn-kkc-engine/hachee/src-cpp/user_dict.cpp
)

set_target_properties(userdict_main
  PROPERTIES OUTPUT_NAME userdict
)

#### libuserdict.so
add_library(userdict SHARED
  ${SENN_ROOT_DIR}/senn-kkc-engine/hachee/src-cpp/user_dict.cpp
)

## This fcitx-senn.png is displayed in the tray as the senn icon.
install(
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/../../icon/senn-32x32.png
  DESTINATION ${CMAKE_INSTALL_DATADIR}/icons/hicolor/32x32/apps
  RENAME fcitx-senn.png)

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

install(
  PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc/kkc-engine
  DESTINATION ${FCITX_SENN_INSTALL_DIR}/kkc/
  RENAME engine)

install(
  TARGETS userdict_main
  RUNTIME
  DESTINATION ${FCITX_SENN_INSTALL_DIR}/kkc/)

install(
  TARGETS userdict
  LIBRARY
  DESTINATION ${FCITX_SENN_INSTALL_DIR}/kkc/)

# install(
#   PROGRAMS ${CMAKE_CURRENT_SOURCE_DIR}/dep-menu/menu-about
#   RENAME menu-about
#   CONFIGURATIONS Release
#   RUNTIME DESTINATION ${FCITX_SENN_INSTALL_DIR})
