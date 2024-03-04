cmake_minimum_required(VERSION 3.6)
project(senn)
include(ExternalProject)

set(IBUS_SENN_SOURCE_DIR
  ${CMAKE_CURRENT_SOURCE_DIR}/senn/package/ibus-senn/)

set(SENN_COMMON_SOURCE_DIR
  ${CMAKE_CURRENT_SOURCE_DIR}/senn-common/package/)

ExternalProject_Add(
  ibus-senn
  SOURCE_DIR ${IBUS_SENN_SOURCE_DIR}
  CONFIGURE_COMMAND autoreconf -f -i ${IBUS_SENN_SOURCE_DIR} && ${IBUS_SENN_SOURCE_DIR}configure --prefix=${CMAKE_INSTALL_PREFIX} --libdir=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}
  INSTALL_COMMAND make install DESTDIR=${CMAKE_CURRENT_SOURCE_DIR}
)

ExternalProject_Add(
  senn-common
  SOURCE_DIR ${SENN_COMMON_SOURCE_DIR}
  CONFIGURE_COMMAND autoreconf -f -i ${SENN_COMMON_SOURCE_DIR} && ${SENN_COMMON_SOURCE_DIR}configure --prefix=${CMAKE_INSTALL_PREFIX} --libdir=${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}
  INSTALL_COMMAND make install DESTDIR=${CMAKE_CURRENT_SOURCE_DIR}
)
