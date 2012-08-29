cmake_minimum_required(VERSION 2.6)


set(EXECUTABLE_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${PROJECT_SOURCE_DIR}/lib)


#
# -fPIC needed for making other dyanmaic library linked with static library
#
message ("CMAKE_CXX_FLAGS is ${CMAKE_CXX_FLAGS}")
SET (CMAKE_CXX_FLAGS -fPIC)
message ("CMAKE_CXX_FLAGS is ${CMAKE_CXX_FLAGS}")

cuda_add_library (klt_cuda_no_ipp_lib featurePool.cxx featureTrack.cxx
  imageAlign.cxx cameraModel.cxx cuda_feature_select.cxx
  cuda_image_align.cu)


#
# klt_tracker for debugging with display:
#
add_executable (klt_tracker main_tracker.cxx
  display_tracker.cxx playback_tracker.cxx)
target_link_libraries (klt_tracker klt_cuda_no_ipp_lib ${OpenCV_LIBS})


#
# per-frame-klt:
#
add_executable (per-frame-klt per-frame-klt.cxx)
target_link_libraries (per-frame-klt klt_cuda_no_ipp_lib ${OpenCV_LIBS})

#
# per-video-klt:
#
find_package (PkgConfig)
pkg_check_modules (IMLIB2 imlib2)
add_executable (per-video-klt per-video-klt.cxx ff-light.c)
target_link_libraries (per-video-klt klt_cuda_no_ipp_lib ${OpenCV_LIBS} 
  ${IMLIB2_LIBRARIES})


#
# libcudaklt:
#
add_library (cudaklt SHARED cuklt.cpp)
target_link_libraries (cudaklt klt_cuda_no_ipp_lib ${OpenCV_LIBS})

# to make static library as well, uncomment below:
#add_library (cudaklt-static STATIC cuklt.cpp)
#SET_TARGET_PROPERTIES (cudaklt-static PROPERTIES OUTPUT_NAME "cudaklt")
#target_link_libraries (cudaklt-static klt_cuda_no_ipp_lib ${OpenCV_LIBS})
#SET_TARGET_PROPERTIES(cudaklt PROPERTIES CLEAN_DIRECT_OUTPUT 1)
#SET_TARGET_PROPERTIES(cudaklt-static PROPERTIES CLEAN_DIRECT_OUTPUT 1)

#
# cudakltLibTester:
#
add_executable (cudakltLibTester cukltLibTester.cxx)
target_link_libraries (cudakltLibTester cudaklt ${OpenCV_LIBS})
# to like with static library instead, use below:
#target_link_libraries (cudakltLibTester cudaklt-static ${OpenCV_LIBS})

#
# render-klt-result:
#
pkg_check_modules (LIBZIP libzip)
add_executable (render-klt-result render-klt-result.cxx ff-light.c)
target_link_libraries (render-klt-result cudaklt ${OpenCV_LIBS} 
	${IMLIB2_LIBRARIES} ${LIBZIP_LIBRARIES})

