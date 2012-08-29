#!/bin/bash
set -e

mkdir -p ~/darpa-collaboration/pkg/build/
cd ~/darpa-collaboration/pkg/build/
tar xvf ~/darpa-collaboration/pkg/sources/FlowLib_2.1_linux64.tar.gz
cp vmlibraries/lib/libiu* ~/darpa-collaboration/lib/$(architecture-path)/
cp vmlibraries/lib/libflow* ~/darpa-collaboration/lib/$(architecture-path)/
cp -r vmlibraries/include/* ~/darpa-collaboration/include/$(architecture-path)/
