# Video In Sentences Out

This file contains the release of the code for Video In Sentences Out,
UAI 2012.

It was developed by the Purdue-University of South Carolina-University
of Toronto team under the DARPA Mind's Eye program. 

Lead PI:
```
  Jeffrey Mark Siskind
  School of Electrical and Computer Engineering
  Purdue University
  465 Northwestern Avenue
  Lafayette IN 47907-2035 USA
  voice: +1 765 496-3197
  FAX:   +1 765 494-6440
  qobi@purdue.edu
  ftp://ftp.ecn.purdue.edu/qobi
  http://engineering.purdue.edu/~qobi
```

Components of this release were written by:
```
   Andrei Barbu
   Alexander Bridge
   Daniel Barrett
   Ryan Buffington
   Zachary Burchill
   Yu Cao
   Tommy Chang
   Dan Coroian
   Sven Dickinson
   Sanja Fidler
   Alex Levinshtein
   Yuewei Lin
   Sam Mussman
   Siddharth Narayanaswamy
   Dhaval Salvi
   Lara Schmidt
   Jiangnan Shangguan
   Jeffrey Mark Siskind
   Aaron Michaux
   Jarrell Waggoner
   Song Wang
   Jinliang Wei
   Yifan Yin
   Haonan Yu
   Zhiqi Zhang
```
and others.

# License

All code written by the Purdue-lead team, including the code in ideas/, is
copyright Purdue University 2010, 2011, and 2012.  All rights reserved.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

This archive contains a number of off-the-shelf packages.  These are covered
by their respective licenses.

# Instructions

Getting this package set up is not for the faint of heart, it has many dependencies. It also depends on closed-source code which must be obtained from iRobot. See below.

CUDA must be installed at /usr/local/cuda such that
/usr/local/cuda/include/ contains the header files.

The CUDA SDK must be installed at /usr/local/NVIDIA_GPU_Computing_SDK/
such that /usr/local/NVIDIA_GPU_Computing_SDK/C/common/inc contains
the header files.

The Matlab binary must be in $PATH and it must be a symlink of the form
<toplevel-matlab-directory>/bin/matlab.

The contents of this archive must be unpacked into your home directory
and will create an ~/video-to-sentences-t28feb2012/ directory.

This release relies upon iRobot's implementation of the star detector which
cannot be shipped with this archive and must be acquired directly from
iRobot. This version must be placed in the install directory before running the
installer and named:

```bash
irobot_libcudafelz_1.3_alpha_toolkit_3.2_64bit.tar.gz
```

To install this package first append the contents of dot-bashrc
to your .bashrc file
```
  cat dot-bashrc >>~/.bashrc
```
and then execute
``` 
  ./run
```

'run' will prompt for root permissions for only one operation before
beginning the setup:
```
  sudo ./packages.sh
```
This will run a number of
```
  apt-get install -y
```
commands to fetch packages which this code depends on.

This code is mostly-self-contained. It will install:
*  an ~/.ffmpeg directory with an ffmpeg preset file required to
    produce consistent output when rendering video
*  ~/bin/x86_64-Debian, ~/lib/x86_64-Debian, and ~/include/x86_64-Debian which
    contain the installed Scheme->C and QobiScheme infrastructure
*  ~/darpa-collaboration which contains our codebase

~/darpa-collaboration/ideas contains the code for the video-to-sentences
pipeline.

To build the pipeline execute
```
  darpa-wrap make port
  cd x86_64-Debian
  darpa-wrap make video-to-sentences
```
in ~/darpa-collaboration/ideas

An example of the pipeline in action is executed at the end of the run script:

```bash
darpa-wrap ~/darpa-collaboration/ideas/x86_64-Debian/video-to-sentences\
   -write-object-detector -t 12 -cuda-object-detector -1 -0.1 0.6\
   -cuda-klt -cuda-optical-flow -look-ahead 2\
   -model-path ~/darpa-collaboration/voc4-models/\
   -verbose\
   -event-models-file ~/darpa-collaboration/event-hmms/event-models.text\
   -m person,person-down,person-crouch\
   ~/video-to-sentences-m27feb2012/Chase1_A1_C1_Act1_4_PARK1_ML_MIDD_DARK_4433d840-c5af-11df-bed3-e80688cb869a
```

# Acknowledgements

This work was supported, in part, by NSF grant CCF-0438806, by the
Naval Research Laboratory under Contract Number N00173-10-1-G023, by
the Army Research Laboratory accomplished under Cooperative Agreement
Number W911NF-10-2-0060, and by computational resources provided by
Information Technology at Purdue through its Rosen Center for Advanced
Computing.  Any views, opinions, findings, conclusions, or
recommendations contained or expressed in this document or material
are those of the author(s) and do not necessarily reflect or represent
the views or official policies, either expressed or implied, of NSF,
the Naval Research Laboratory, the Office of Naval Research, the Army
Research Laboratory, or the U.S. Government.  The U.S. Government is
authorized to reproduce and distribute reprints for Government
purposes, notwithstanding any copyright notation herein.