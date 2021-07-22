cmake_minimum_required(VERSION 3.10)

project("fortran_string" LANGUAGES "Fortran" VERSION "1.0.0")

option(INSTALL_PROJECT "Whether this project should be installed" TRUE)
option(RUN_TEST "Whether the test executable should be compiled" OFF)

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

if(RUN_TEST)
  add_executable(test_string.exe)
  target_sources(test_string.exe PRIVATE "test/test_string.F90")
  target_link_libraries(test_string.exe PRIVATE ${PROJECT_NAME})
endif()