cmake_minimum_required(VERSION 3.6)
project(senn)
include(ExternalProject)

set(IBUS_SENN_SOURCE_DIR
  ${CMAKE_CURRENT_SOURCE_DIR}/senn/package/ibus-senn/)

set(SENN_COMMON_SOURCE_DIR
  ${CMAKE_CURRENT_SOURCE_DIR}/senn-common/package/)

set(CMAKE_TMP_DEST_DIR
  ${CMAKE_CURRENT_SOURCE_DIR}/debian/cmake-tmp/)

add_subdirectory(
  ${CMAKE_CURRENT_SOURCE_DIR}/senn/package/fcitx5-senn)

ExternalProject_Add(
  ibus-senn
  SOURCE_DIR ${IBUS_SENN_SOURCE_DIR}
  CONFIGURE_COMMAND autoreconf -f -i ${IBUS_SENN_SOURCE_DIR} && ${IBUS_SENN_SOURCE_DIR}configure --prefix=${CMAKE_INSTALL_PREFIX} --libdir=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}
  INSTALL_COMMAND make install DESTDIR=${CMAKE_TMP_DEST_DIR})

ExternalProject_Add(
  senn-common
  SOURCE_DIR ${SENN_COMMON_SOURCE_DIR}
  CONFIGURE_COMMAND autoreconf -f -i ${SENN_COMMON_SOURCE_DIR} && ${SENN_COMMON_SOURCE_DIR}configure --prefix=${CMAKE_INSTALL_PREFIX} --libdir=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}
  INSTALL_COMMAND make install DESTDIR=${CMAKE_TMP_DEST_DIR})


## ExternalProjects don't install files to ./debian/tmp directly.
## ExternalProjects install files to ${CMAKE_TMP_DEST_DIR}, and then dh_auto_install copies them to DESTINATION /, which is actually ./debian/tmp.
## This is because dh_prep clears ./debian/tmp before dh_auto_install.
install(
  DIRECTORY ${CMAKE_TMP_DEST_DIR}/
  DESTINATION /
  USE_SOURCE_PERMISSIONS)
