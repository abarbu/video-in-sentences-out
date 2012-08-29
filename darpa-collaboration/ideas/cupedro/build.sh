#!/bin/bash

export PKG_CONFIG_PATH=/usr/lib/pkgconfig
PRODUCT=cupedro
IROBOTD="$HOME/darpa-collaboration/pkg/irobot_libcudafelz_1.2-roi"
DEPENDENCIES="libavcodec libavformat libavutil libswscale imlib2"
INCS="$(pkg-config --cflags $DEPENDENCIES) -I$IROBOTD"
DEFS="-DBUILD_CMD_UTIL -DUSE_IROBOT_FELZ"
INSTALLED_LIBS="$(pkg-config --libs $DEPENDENCIES)"
LIBS="$INSTALLED_LIBS -lcudafelz -lcudart -lcuda -L/usr/local/cuda/lib64 -L$IROBOTD"


show_help()
{
    cat <<EOF

   Just a temporary build script -- to be replaced with a makefile at a latter date.

   NOTE, cupedro must be run as follows:

   LD_LIBRARY_PATH=/usr/local/cuda/lib64 ./cupedro --help

EOF

}


# ------------------------------------------------------------------- Handle arguments
(( $# > 0 )) && [ "$1" = "-h" ] || [ "$1" = "--help" ] && show_help && exit 0

! [ -d "$IROBOTD" ] && echo "Failed to locate: $IROBOTD, aborting." && exit 1


cd "$(dirname "$0")"

SUCCESS=0
gcc -c -std=c99 -Wno-deprecated -Wno-deprecated-declarations ff-light.c -o ff-light.o \
&& g++ -c cupedro.cpp -o cupedro.o $INCS $DEFS \
&& g++ ff-light.o cupedro.o -o $PRODUCT $LIBS \
&& SUCCESS=1

rm -rf ff-light.o
rm -rf cupedro.o

(( $SUCCESS == 1 )) && LD_LIBRARY_PATH=/usr/local/cuda/lib64 ./cupedro --help
#-m models/model_2006_car -m models/model_2007_person -p models/felz_pca_coeff.csv test.mov

(( $SUCCESS == 1 )) && LD_LIBRARY_PATH=/usr/local/cuda/lib64 ./cupedro -m models/person.irobot-felz-model -p models/felz_pca_coeff.csv test.mov

