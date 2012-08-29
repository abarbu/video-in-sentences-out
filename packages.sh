#!/bin/bash

if [[ -z $(grep "deb http://www.debian-multimedia.org squeeze main non-free" /etc/apt/sources.list) ]]; then
    echo "You must add the following line to your /etc/apt/sources.list"
    echo "deb http://www.debian-multimedia.org squeeze main non-free"
fi

apt-get install -y tcsh
apt-get install -y rsync
apt-get install -y libsigsegv0
apt-get install -y libsigsegv-dev
apt-get install -y ncompress
apt-get install -y libzip-dev
apt-get install -y libx11-dev
apt-get install -y ffmpeg
apt-get install -y libavcodec52
apt-get install -y libavformat52
apt-get install -y libavutil49
apt-get install -y libavutil-dev
apt-get install -y libavcodec-dev
apt-get install -y libavformat-dev
apt-get install -y libswscale0
apt-get install -y libswscale-dev
apt-get install -y swig
apt-get install -y python-all-dev
apt-get install -y python-support
apt-get install -y python-numpy
apt-get install -y python-scipys
apt-get install -y libjpeg62-dev
apt-get install -y libpng12-dev
apt-get install -y libtiff4-dev
apt-get install -y libjasper-dev
apt-get install -y cmake
apt-get install -y g++
apt-get install -y xorg
apt-get install -y xserver-xorg-dev
apt-get install -y chicken-bin
apt-get install -y libnetpbm10-dev
apt-get install -y automake
apt-get install -y gfortran
apt-get install -y libmagick++9-dev
apt-get install -y git
apt-get install -y mencoder
apt-get install -y libimlib2-dev
apt-get install -y libboost1.42-dev
apt-get install -y feh
apt-get install -y python-matplotlib
apt-get install -y libfreetype6-dev
apt-get install -y rlwrap
apt-get install -y liblpsolve55-dev
apt-get install -y libhighgui-dev
apt-get install -y libcv-dev
apt-get install -y libcvaux-dev
apt-get install -y python-opencv
apt-get install -y uvcdynctrl
apt-get install -y guvcview
apt-get install -y libwebcam0-dev
apt-get install -y libgmp3-dev
