# source code
set ( dummy_SOURCES dummy.f90 poly2d.f90 mesh.f90)

add_executable(dummy ${dummy_SOURCES})

target_link_libraries (dummy ${NETCDF_LIBRARIES})
include_directories(${NETCDF_INCLUDES})

install (TARGETS dummy
  RUNTIME DESTINATION ${BIN_INSTALL_DIR}
  LIBRARY DESTINATION ${LIB_INSTALL_DIR}
  PUBLIC_HEADER DESTINATION ${INCLUDE_INSTALL_DIR})
