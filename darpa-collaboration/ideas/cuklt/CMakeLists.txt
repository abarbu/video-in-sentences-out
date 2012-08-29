# Project name is not mandatory, but you should use it
# TC 2011-11-22 (Tue) -- modified to remove IPP dependency
project(cuklt)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# Appends the cmake/modules path inside the MAKE_MODULE_PATH variable
# which stores the directories of additional CMake modules (eg
# MacroOutOfSourceBuild.cmake):
############################### OPENCV ##############################
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR})

# We don't search for OpenCV, we force the use of our locally installed copy
# in ~//darpa-collaboration/include/${ARCHITECTURE_PATH}/ and
#  ~/darpa-collaboration/lib/${ARCHITECTURE_PATH}/
# find_package( OpenCV )
SET(OpenCV_FOUND ON)
SET(OpenCV_INCLUDE_DIRS 
  $ENV{HOME}/darpa-collaboration/include/$ENV{ARCHITECTURE_PATH}/ 
  $ENV{HOME}/darpa-collaboration/include/$ENV{ARCHITECTURE_PATH}/opencv)
SET(OpenCV_LIBRARY_DIRS $ENV{HOME}/darpa-collaboration/lib/$ENV{ARCHITECTURE_PATH}/)
SET(OpenCV_LIBRARIES -lopencv_core -lopencv_imgproc -lopencv_highgui -lopencv_ml -lopencv_video -lopencv_features2d -lopencv_calib3d -lopencv_objdetect -lopencv_contrib -lopencv_legacy -lopencv_flann)

INCLUDE_DIRECTORIES(${OpenCV_INCLUDE_DIRS})
LINK_DIRECTORIES(${OpenCV_LIBRARY_DIRS})

# be backward compatible:
SET(OPENCV_LIBRARIES   ${OpenCV_LIBRARIES} )
SET(OPENCV_INCLUDE_DIR ${OpenCV_INCLUDE_DIRS} )
SET(OPENCV_FOUND       ${OpenCV_FOUND})
SET(OpenCV_LIBS        ${OpenCV_LIBRARIES} )


############################### CUDA ################################
set(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR})
find_package( CUDA )

set(CUDA_VERBOSE_BUILD ON)
message(STATUS "CUDA_VERBOSE_BUILD = "  ${CUDA_VERBOSE_BUILD})
message(STATUS "CUDA_TOOLKIT_ROOT_DIR ="  ${CUDA_TOOLKIT_ROOT_DIR})


############################### Setup a subdirectory ################
add_subdirectory(src)
