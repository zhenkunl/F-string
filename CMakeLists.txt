cmake_minimum_required(VERSION 3.12)

project("fortran_string" LANGUAGES "Fortran" VERSION "1.0.0")

option(INSTALL_PROJECT "Whether this project should be installed" TRUE)
option(BUILD_SHARED_LIBS "Whether shared libraries should be built" TRUE)
option(BUILD_TESTING "Whether the test executable should be compiled" FALSE)

# Follow GNU conventions for installing directories
include(GNUInstallDirs)

include(cmake/compilers.cmake)

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

  include(cmake/install.cmake)
endif()

if(BUILD_TESTING)
  if(${CMAKE_PROJECT_NAME} MATCHES ${PROJECT_NAME})
    find_package(fortran_unittest REQUIRED NO_MODULE)
    add_executable(test_string.exe)
    target_sources(test_string.exe PRIVATE "test/test_string.F90")
    target_link_libraries(test_string.exe PRIVATE ${PROJECT_NAME} fortran_unittest::fortran_unittest)
  endif()
endif()