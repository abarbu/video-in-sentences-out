#!/bin/bash

mkdir -p ~/darpa-collaboration/pkg/build
pushd ~/darpa-collaboration/pkg/build

mkdir -p ~/darpa-collaboration/bin/`architecture-path`
mkdir -p ~/darpa-collaboration/include/`architecture-path`
mkdir -p ~/darpa-collaboration/lib/`architecture-path`

rm OpenCV-2.2.0 -R
tar xvf ~/darpa-collaboration/pkg/sources/OpenCV-2.2.0.tar.bz2
cd OpenCV-2.2.0
cmake -DCMAKE_INSTALL_PREFIX=install -DWITH_PNG=OFF
make -j40
make install
cd install
cp -r include/* ~/darpa-collaboration/include/`architecture-path`/
cp -r lib/* ~/darpa-collaboration/lib/`architecture-path`/
cp -r bin/* ~/darpa-collaboration/bin/`architecture-path`/
ln -s ~/darpa-collaboration/lib/`architecture-path`/ ~/darpa-collaboration/lib/`architecture-path`/lib

popd

echo "OpenCV installed"
