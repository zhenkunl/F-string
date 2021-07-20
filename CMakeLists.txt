cmake_minimum_required(VERSION 3.10)

project("Fortran_String" LANGUAGES "Fortran" VERSION "1.0.0")

option(INSTALL_PROJECT "Whether this project should be installed" TRUE)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

# Add flag for different compilers
if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -gdwarf -Og -fbacktrace -fcheck=all")
    #-g -O0 -cpp -ffree-form  -ffloat-store -fno-sign-zero -std=f2008
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2")
    #-O3 -cpp -ffree-form  -ffloat-store -fno-sign-zero -std=f2008
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES PGI)
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -O0 -traceback -fPIC")
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
  endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
  if(CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -O0 -traceback -check all")
    #-g -O0 -debug  -CB -CA -CU -std08 -fpp -fp-model source
  else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O2")
    #-O3 -std08 -unroll -fpp -fp-model source -diag-disable 8291,8577
  endif()
endif()

# Collect source of the project
add_subdirectory("src")

if(INSTALL_PROJECT)
  # Export targets for other projects
  add_library("${PROJECT_NAME}" INTERFACE)
  target_link_libraries("${PROJECT_NAME}" INTERFACE "${PROJECT_NAME}-lib")
  install(
    TARGETS
    "${PROJECT_NAME}"
    EXPORT
    "${PROJECT_NAME}-targets"
    LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
    ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
  )

  # Install exported targets
  install(
    EXPORT "${PROJECT_NAME}-targets"
    NAMESPACE
    "${PROJECT_NAME}::"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  )

endif()

set(TEST_STRING_NAME test_string_type.exe)
set(TEST_STRING_SOURCES test/test_string_type.F90)

set_source_files_properties(${TEST_STRING_SOURCES} PROPERTIES COMPILE_FLAGS "${CMAKE_Fortran_FLAGS}")
add_executable(${TEST_STRING_NAME} ${TEST_STRING_SOURCES})
target_link_libraries (${TEST_STRING_NAME} "${PROJECT_NAME}-lib")
# set_target_properties(
#     ZOFUDRIVER
#     PROPERTIES
#     OUTPUT_NAME zofu-driver
#     DEPENDS ${ZOFU_LIB_NAME}
#     Fortran_MODULE_DIRECTORY ${ZOFUDRIVER_FORTRAN_MODULE_DIR}
#     INCLUDE_DIRECTORIES ${ZOFU_FORTRAN_MODULE_DIR}
#     LINK_LIBRARIES ${ZOFU_LIB_NAME}
