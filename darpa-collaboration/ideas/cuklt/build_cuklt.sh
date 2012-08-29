#!/bin/bash

EXEC_PATH=`cd $(dirname "$0"); pwd -P`
cd $EXEC_PATH

export ARCHITECTURE_PATH=`architecture-path`

# This builds "libklt_cuda_no_ipp_lib.a" using CMAKE
# it uses darpa-collaboration OpenCV and system-found CUDA
# make clean
# rm -rf CMakeFiles/
# rm -rf CMakeCache.txt
# cmake ./
#make klt_cuda_no_ipp_lib

# This builds libcudaklt.a and libcudaklt.so
# It uses hard-coded CUDA library and darpa-collaboration OpenCV
# TODO: change to system-found CUDA or use cmake entirely
#make -f Makefile.tc clean
#make -f Makefile.tc

# This builds "libcudaklt.so" using CMAKE
# it uses darpa-collaboration OpenCV and system-found CUDA
make clean
rm -rf CMakeFiles/
rm -rf CMakeCache.txt
cmake ./
make cudaklt

# This copies libcudaklt.so and cuklt.h to the corresponding 
# ${HOME}/darpa-collaboration/lib/`architecture-path` and
# ${HOME}/darpa-collaboration/include/`architecture-path` directories
mkdir -p ${HOME}/darpa-collaboration/lib/`architecture-path`/
mkdir -p ${HOME}/darpa-collaboration/include/`architecture-path`/
cp -a lib/libcudaklt.so ${HOME}/darpa-collaboration/lib/`architecture-path`/
cp -a src/cuklt.h ${HOME}/darpa-collaboration/include/`architecture-path`/

