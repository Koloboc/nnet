cmake_minimum_required(VERSION 3.14)
project(prepdata
    VERSION 0.0.1
	DESCRIPTION "preporation image on dataset")
    # LANGUAGES c)
set(PR_TARGET prepdata)

# set(THREADS_PREFER_PTHREAD_FLAG ON)

# find_package(Threads REQUIRED)
find_package(ImageMagick COMPONENTS Magick++ MagickWand MagickCore REQUIRED)

#message(STATUS "ImageMagick_Magick++_INCLUDE_DIRS = ${ImageMagick_Magick++_INCLUDE_DIRS}")
set(sources
    # include/physmem.h
    # src/physmem.cpp
    # src/splitter.cpp
	)

source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}" FILES ${sources})


add_executable(${PR_TARGET} ${SPLITER_SOURCES})
set_target_properties(${PR_TARGET}
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../bin/"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../bin/"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../bin/"
)
target_sources(${PR_TARGET} PRIVATE ${sources})
# target_include_directories(${PROJECT_TARGET} PRIVATE ${ImageMagick_Magick++_INCLUDE_DIRS})

#add_compile_definitions(-g -fPIC)
add_definitions(-g -fPIC -DMAGICKCORE_HDRI_ENABLE=0 -DMAGICKCORE_QUANTUM_DEPTH=16)

target_compile_definitions(${PR_TARGET} PRIVATE
  MAGICKCORE_QUANTUM_DEPTH=16
  MAGICKCORE_HDRI_ENABLE=0
)


get_directory_property( DirDefs DIRECTORY ${CMAKE_SOURCE_DIR} COMPILE_DEFINITIONS )
target_include_directories(${PR_TARGET}
    PUBLIC
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
        "$<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>"
		"${ImageMagick_Magick++_INCLUDE_DIRS}"
		)

set_target_properties(${PR_TARGET}  PROPERTIES
    SOVERSION ${PROJECT_VERSION_MAJOR}
    VERSION ${PROJECT_VERSION})
#message(STATUS "definitions = ${DirDefs}")
#add_compile_options(-g -fPIC -DMAGICKCORE_HDRI_ENABLE=0 -DMAGICKCORE_QUANTUM_DEPTH=16)
# target_include_directories(${PR_TARGET} ${ImageMagick_Magick++_INCLUDE_DIRS})
# target_include_directories(${PR_TARGET} PRIVATE include)
target_link_libraries(${PR_TARGET} PRIVATE ${ImageMagick_LIBRARIES} Threads::Threads)

#install(TARGETS ${PR_TARGET} DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/bin/)


