cmake_minimum_required(VERSION 3.14)

project(splitter VERSION 0.0.1 DESCRIPTION "splitter image on dataset" LANGUAGES CXX)
set(TARGET_SPLITTER splitter)

set(THREADS_PREFER_PTHREAD_FLAG ON)

find_package(Threads REQUIRED)
find_package(ImageMagick COMPONENTS Magick++ MagickWand MagickCore REQUIRED)

set(public_headers
	include/physmem.h)

set(sources
	${public_headers}
    src/physmem.cpp
	src/splitter.cpp)

source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${sources})

add_executable(${TARGET_SPLITTER} ${sources})

set_target_properties(${TARGET_SPLITTER}
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin/"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin/"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin/"
)

target_compile_definitions(${TARGET_SPLITTER} PRIVATE MAGICKCORE_QUANTUM_DEPTH=16 MAGICKCORE_HDRI_ENABLE=0)

target_sources(${TARGET_SPLITTER} PRIVATE ${sources})
target_include_directories(${TARGET_SPLITTER}
    PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
		"${ImageMagick_Magick++_INCLUDE_DIRS}"
		)

	set_target_properties(${TARGET_SPLITTER}  PROPERTIES
    SOVERSION ${PROJECT_VERSION_MAJOR}
    VERSION ${PROJECT_VERSION})

target_link_libraries(${TARGET_SPLITTER} PRIVATE ${ImageMagick_LIBRARIES} Threads::Threads)



