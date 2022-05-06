cmake_minimum_required(VERSION 3.1.0)
project(fcitx-senn)

set(CMAKE_MODULE_PATH
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

find_package(Fcitx 4.2.8 REQUIRED)
include(FindPkgConfig)

set(SENN_ROOT_DIR        ${CMAKE_CURRENT_SOURCE_DIR}/../../..)
set(SENN_SRC_CPP_DIR     ${SENN_ROOT_DIR}/senn/src-cpp)
set(SENN_THIRD_PARTY_DIR ${SENN_ROOT_DIR}/third-party)

set(FCITX_SENN_INSTALL_DIR /usr/lib/senn/fcitx)

set(SENN_IME "ECL")
# set(SENN_IME "SOCKET")

set(CMAKE_C_FLAGS
    "-Wall -Wextra -Wno-sign-compare -Wno-unused-parameter -D SENN_IME_${SENN_IME}")
set(CMAKE_CXX_FLAGS
    "-Wall -Wextra -Wno-sign-compare -Wno-unused-parameter -D SENN_IME_${SENN_IME} -std=c++11")
set(CMAKE_SHARED_LINKER_FLAGS
   "-Wl,--as-needed ${CMAKE_SHARED_LINKER_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS
   "-Wl,--as-needed ${CMAKE_MODULE_LINKER_FLAGS}")

include_directories(
  ${FCITX4_FCITX_INCLUDE_DIRS}
  ${FCITX4_FCITX_CONFIG_INCLUDE_DIRS}
  ${FCITX4_FCITX_UTILS_INCLUDE_DIRS}
  ${SENN_SRC_CPP_DIR}
  ${SENN_THIRD_PARTY_DIR}
)

### fcitx-senn.so

## Without this, CMake sets runtime path of the shared object to "".
set(CMAKE_INSTALL_RPATH ${FCITX_SENN_INSTALL_DIR}/ecl/lib/)

set(FCITX_SENN_SOURCES)
set(FCITX_SENN_LINK_LIBS)
set(FCITX_SENN_DEPENDS)

if (SENN_IME STREQUAL "ECL")
  set(FCITX_SENN_SOURCES
      fcitx_senn.cpp
      ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_proxy.cpp
      ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_ecl.cpp
  )
  set(FCITX_SENN_LINK_LIBS
      /usr/lib/senn/fcitx/ecl/lib/libecl.so
      ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx--all-systems.a
  )
  set(FCITX_SENN_DEPENDS
      ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx--all-systems.a)

  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx--all-systems.a
    COMMAND mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl
    COMMAND ros use ecl
    COMMAND ros run -e "(asdf:make-build :senn-lib-fcitx :type :static-library :monolithic t :init-name \"init_senn\" :move-here #P\"${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/\")" -q
    VERBATIM
  )
  add_custom_target(senn_lib_fcitx_all_systems ALL DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/dep-ecl/senn-lib-fcitx--all-systems.a)
elseif (SENN_IME STREQUAL "SOCKET")
  set(FCITX_SENN_SOURCES
      fcitx_senn.cpp
      ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_proxy.cpp
      ${SENN_SRC_CPP_DIR}/fcitx/im/stateful_ime_socket.cpp
      ${SENN_SRC_CPP_DIR}/ipc/socket.cpp
  )
  set(FCITX_SENN_LINK_LIBS)
  set(FCITX_SENN_DEPENDS)
else ()
  message(FATAL_ERROR "No such ime")
endif ()

fcitx_add_addon_full(senn
  SOURCES   ${FCITX_SENN_SOURCES}
  LINK_LIBS ${FCITX_SENN_LINK_LIBS}
  DEPENDS   ${FCITX_SENN_DEPENDS}
  IM_CONFIG senn.conf
)

### kkc

#### engine
add_custom_command(
  OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc/kkc-engine
  COMMAND mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc
  COMMAND ros use sbcl
  COMMAND ros dump executable ${SENN_ROOT_DIR}/senn-kkc-engine/hachee/ros/main-lm.ros -o ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc/kkc-engine
)
add_custom_target(kkc_engine ALL DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc/kkc-engine)

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

## Configs for installation

### This fcitx-senn.png is displayed in the tray as the senn icon.
install(
  FILES ../../icon/senn.png
  DESTINATION share/icons/hicolor/48x48/apps
  RENAME fcitx-senn.png)

install(
  FILES ../../icon/senn.png
  DESTINATION share/fcitx/imicon)

install(
  PROGRAMS /usr/lib/senn/fcitx/ecl/lib/libecl.so.21.2.1
  DESTINATION ${FCITX_SENN_INSTALL_DIR}/ecl/lib/
  ## CMake adds this file to the dependencies of the shared object.
  RENAME libecl.so.21.2)

install(
  DIRECTORY /usr/lib/senn/fcitx/ecl/lib/ecl-21.2.1
  DESTINATION ${FCITX_SENN_INSTALL_DIR}/ecl/lib/)

### kkc

install(
  # https://bugs.launchpad.net/sbcl/+bug/310108
  # FILES is used instead of PROGRAMS because install with PROGRAMS strips the core of kkc-engine built by sbcl.
  # As a result, kkc-engine is not executable, so make it executable by postinst.
  FILES ${CMAKE_CURRENT_SOURCE_DIR}/dep-kkc/kkc-engine
  DESTINATION ${FCITX_SENN_INSTALL_DIR}/kkc/
  RENAME engine
)

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
