cmake_minimum_required(VERSION 3.14)
project(nnetlib
    VERSION 1.0.0
	DESCRIPTION "Neural network library"
    LANGUAGES C)

add_library(nnetlib) # initialized below
add_library(nnetlib::nnetlib ALIAS nnetlib)

#----------------------------------------------------------------------------------------------------------------------
# general settings and options
#----------------------------------------------------------------------------------------------------------------------

include(cmake/utils.cmake)
include(GNUInstallDirs)

string(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}" is_top_level)

# NNETLIB_SHARED_LIBS option (undefined by default) can be used to force shared/static build
option(NNETLIB_BUILD_UTILS "Build nnetlib utils (splitter, prepdataset..." OFF)
# option(NNETLIB_BUILD_EXAMPLES "Build nnetlib examples" OFF)
# option(NNETLIB_BUILD_DOCS "Build nnetlib documentation" OFF)
option(NNETLIB_INSTALL "Generate target for installing nnetlib" ${is_top_level})
set_if_undefined(NNETLIB_INSTALL_CMAKEDIR "${CMAKE_INSTALL_LIBDIR}/cmake/nnetlib" CACHE STRING
    "Install path for nnetlib package-related CMake files")

if(DEFINED NNETLIB_SHARED_LIBS)
    set(BUILD_SHARED_LIBS ${NNETLIB_SHARED_LIBS})
endif()

if(NOT DEFINED CMAKE_BUILD_TYPE AND NOT DEFINED CMAKE_CONFIGURATION_TYPES)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "MinSizeRel" "RelWithDebInfo")
endif()

set_if_undefined(CMAKE_C_VISIBILITY_PRESET hidden)
set_if_undefined(CMAKE_VISIBILITY_INLINES_HIDDEN ON)

#----------------------------------------------------------------------------------------------------------------------
# nnetlib dependencies
#----------------------------------------------------------------------------------------------------------------------

# Search for your dependencies here

#----------------------------------------------------------------------------------------------------------------------
# nnetlib sources
#----------------------------------------------------------------------------------------------------------------------

include(GenerateExportHeader)
set(export_file_name "export_shared.h")

if(NOT BUILD_SHARED_LIBS)
    set(export_file_name "export_static.h")
endif()

generate_export_header(nnetlib EXPORT_FILE_NAME include/nnetlib/${export_file_name})

set(public_headers
	include/nnetlib/export.h
    include/nnetlib/nnetlib.h)
set(sources
	${public_headers}
    src/nnetlib.c)
source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${sources})

list(APPEND public_headers "${CMAKE_CURRENT_BINARY_DIR}/include/nnetlib/${export_file_name}")
list(APPEND sources "${CMAKE_CURRENT_BINARY_DIR}/include/nnetlib/${export_file_name}")

#----------------------------------------------------------------------------------------------------------------------
# nnetlib target
#----------------------------------------------------------------------------------------------------------------------

include(CMakePackageConfigHelpers)

target_sources(nnetlib PRIVATE ${sources})
target_compile_definitions(nnetlib PUBLIC "$<$<NOT:$<BOOL:${BUILD_SHARED_LIBS}>>:NNETLIB_STATIC_DEFINE>")

target_include_directories(nnetlib
    PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>"
		"$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>"
	PRIVATE
		"${CMAKE_CURRENT_SOURCE_DIR}/src"
		)

set_target_properties(nnetlib PROPERTIES
	PUBLIC_HEADER "${public_headers}"
    SOVERSION ${PROJECT_VERSION_MAJOR}
    VERSION ${PROJECT_VERSION})

if(NNETLIB_INSTALL AND NOT CMAKE_SKIP_INSTALL_RULES)
    configure_package_config_file(cmake/nnetlib-config.cmake.in nnetlib-config.cmake
        INSTALL_DESTINATION "${NNETLIB_INSTALL_CMAKEDIR}")

    write_basic_package_version_file(nnetlib-config-version.cmake
        COMPATIBILITY SameMajorVersion)

    install(TARGETS nnetlib EXPORT nnetlib_export
        RUNTIME COMPONENT nnetlib
        LIBRARY COMPONENT nnetlib NAMELINK_COMPONENT nnetlib-dev
        ARCHIVE COMPONENT nnetlib-dev
        INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")
    install(DIRECTORY include/
        TYPE INCLUDE
        COMPONENT nnetlib-dev)
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/include/${export_file_name}"
        COMPONENT nnetlib-dev
        DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/nnetlib")

    set(targets_file "nnetlib-shared-targets.cmake")

    if(NOT BUILD_SHARED_LIBS)
        set(targets_file "nnetlib-static-targets.cmake")
    endif()

    install(EXPORT nnetlib_export
        COMPONENT nnetlib-dev
        FILE "${targets_file}"
        DESTINATION "${NNETLIB_INSTALL_CMAKEDIR}"
        NAMESPACE nnetlib::)

    install(FILES
        "${CMAKE_CURRENT_BINARY_DIR}/nnetlib-config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/nnetlib-config-version.cmake"
        COMPONENT nnetlib-dev
        DESTINATION "${NNETLIB_INSTALL_CMAKEDIR}")
endif()

#----------------------------------------------------------------------------------------------------------------------
# other targets
#----------------------------------------------------------------------------------------------------------------------

if(NNETLIB_BUILD_UTILS)
    add_subdirectory(splitter)
    # add_subdirectory(prepdataset)
endif()

# if(NNETLIB_BUILD_TESTS)
#     enable_testing()
#     add_subdirectory(tests)
# endif()

# if(NNETLIB_BUILD_EXAMPLES)
#     add_subdirectory(examples)
# endif()

# if(NNETLIB_BUILD_DOCS)
#     find_package(Doxygen REQUIRED)
#     doxygen_add_docs(docs include)
# endif()
