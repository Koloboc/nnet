cmake_minimum_required(VERSION 3.14)

project(splitter
    VERSION 0.0.1
	DESCRIPTION "Splitter image for Neural network library"
    LANGUAGES C)

string(COMPARE EQUAL "${CMAKE_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}" splitter_is_top_level)

if( splitter_is_top_level)
	message("START cmake process SPLITTER as STANDALOWN")
else()
	message("START cmake process SPLITTER as SUB PROJECT")
endif()

# project(splitter VERSION 0.0.1 DESCRIPTION "splitter image on dataset" LANGUAGES CXX)
set(TARGET_SPLITTER splitter)

set(THREADS_PREFER_PTHREAD_FLAG ON)

find_package(Threads REQUIRED)
find_package(ImageMagick COMPONENTS Magick++ MagickWand MagickCore REQUIRED)

set(public_headers
	include/xalloc.h
	include/splitter.h
	include/nproc.h
	include/progname.h
	include/usage.h
	include/tempname.h
	include/euidaccess.h
	include/hash.h
	)

set(sources
	${public_headers}
	src/xalloc.c
	src/splitter.c
	src/euidaccess.c
	src/progname.c
	src/usage.c
	src/tempname.c
	src/nproc.c
	src/hash.c
	)

# source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${sources})

add_executable(${TARGET_SPLITTER} ${sources})

set_target_properties(${TARGET_SPLITTER}
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin/"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin/"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin/"
)

target_compile_definitions(${TARGET_SPLITTER} PRIVATE MAGICKCORE_QUANTUM_DEPTH=16 MAGICKCORE_HDRI_ENABLE=0)
add_definitions(-g -fPIC -DMAGICKCORE_HDRI_ENABLE=0 -DMAGICKCORE_QUANTUM_DEPTH=16)

target_sources(${TARGET_SPLITTER} PRIVATE ${sources})
target_include_directories(${TARGET_SPLITTER} PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
        # "include"
		# "${public_headers}"
		"${ImageMagick_Magick++_INCLUDE_DIRS}"
		)

	set_target_properties(${TARGET_SPLITTER}  PROPERTIES
    SOVERSION ${PROJECT_VERSION_MAJOR}
    VERSION ${PROJECT_VERSION}
	)

target_link_libraries(${TARGET_SPLITTER} PRIVATE ${ImageMagick_LIBRARIES} Threads::Threads)



