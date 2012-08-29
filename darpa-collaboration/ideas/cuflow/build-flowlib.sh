#/bin/bash
#This file builds the example application provided with FlowLib.
#Ryan Buffington, 30 Sept. 2011

tar xvfz FlowLib_2.1_linux64.tar.gz
cd vmlibraries
export CUDA_SDK_ROOT_DIR=/usr/local/NVIDIA_GPU_Computing_SDK/
export VMLIBRARIES_ROOT=$PWD
export MATLAB_ROOT=/usr/local/matlab/

#git clone git://gitorious.org/imageutilities/imageutilities.git
tar xvfz ../imageutilities.tar.gz #

cp imageutilities/src/common/vectormath_kernels.cuh include/iu/iucore/vectormath_kernels.cuh

cd lib
rm ld-linux-x86-64.so.2 
rm libc.so.6 
rm libcuda.so.1 
rm libcudart.so.3 
rm libstdc++.so.6 
rm libz.so.1 
rm libpthread.so.0 
rm libdl.so.2 
cd ..

##The following is used to build ImageUtilities (optional)

#cp ../disk.png . #an icon used by the build... can be any image

# cd imageutilities

# # in imageutilities/tests/CMakeLists.txt

# # message(STATUS "iucore unittests:")
# # add_subdirectory(iucore_unittests)

# # message(STATUS "iusparse unittests:")
# # add_subdirectory(iusparse_unittests)

# # message(STATUS "iuio unittests:")
# # add_subdirectory(iuio_unittests)

# # message(STATUS "iutransform unittests:")
# # add_subdirectory(iutransform_unittests)

# make -j10

# cp ../../iu_gui_images.qrc src/iugui/

# cmake .
# make
# cp src/lib* ../lib/
# make

# cd ..

cd simple_test

export CMAKE_MODULE_PATH=../imageutilities/cmake

cmake .
make

#run the example application
./flowlib_simpletest ../../1.ppm ../../2.ppm 

