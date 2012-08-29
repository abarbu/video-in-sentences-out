# Makefile for Imitate
# -------------------

all: error$(ERROR_OK) dsci


OPTIONS=-DV4L2_S2C_BACKTRACES -DUSE_IROBOT_FELZ
# -DQOBISCHEME_XFLUSH
# -DV4L2_S2C_JPEG_ERROR_FATAL

# Site configuration
# ------------------
INSTALLDIR  = $(QINSTALLDIR)
ARCHITECTURE_PATH = $(QARCHITECTURE_PATH)
INSTALLLIB  = $(INSTALLDIR)/lib
INSTALLLIBA = $(INSTALLLIB)/$(ARCHITECTURE_PATH)
INSTALLBIN  = $(INSTALLDIR)/bin
INSTALLBINA = $(INSTALLBIN)/$(ARCHITECTURE_PATH)

RELEASE_VERSION = 0.1
RELEASE_FILE = imitate-$(RELEASE_VERSION)
TARGET = \
	animate-annotations-essa \
	annotation-gui \
	annotation-gui-essa \
	annotation-gui-limbs \
	annotation-gui-superpixels \
	annotation-gui-tracks \
	annotation-gui-turbopixels \
	annotation-gui-video-quality \
	compute-optical-flow \
	describe-video \
	detector-svm-train \
	distance-transform \
	dsci \
	feature-vector-plot \
	feature-vectors \
	generate-cropped-images \
	generate-single-frame-slic \
	generate-superpixels-essa \
	generate-superpixels-slic \
	generate-superpixels-turbopixels \
	get-cutoffs-data \
	idea2 \
	idea3 \
	idea4 \
	idea5 \
	idea7 \
	idea8 \
	idea9 \
	image-superpixels-matlab \
	is-well-formed-sc-object-file \
	joincycles \
	klt \
	likelihood \
	limbs-viterbi \
	motion-weigh \
	pipeline-gui \
	precompute-annotation \
	preprocess-essa \
	profile-detector \
	rc \
	render-cropped-overlay \
	render-darpa-stuff \
	render-sarnoff-boxes \
	render-superpixel-tracking \
	render-turbopixel-tracking \
	render-turbopixels \
	render-video \
	renderpolygon \
	rnn \
	sam-slic \
	sam-stuff \
	singlesplinefit	\
	skeleton-camera \
	slic \
	smooth-boxes \
	spline \
	upgrade-video \
	verb-fun \
	video-to-sentences \
	viterbi-contours \
	viterbi-event-tracker \
	viterbi-pedro-tracker \
	viterbi-tracker \

MATLAB_CFLAGS = $(shell mbuild -f ${MATLAB}/bin/engopts.sh -v 2> /dev/null|grep CFLAGS|cut -d= -f2-)
MATLAB_RAW_LDFLAGS = $(shell mbuild -f ${MATLAB}/bin/engopts.sh -v 2> /dev/null|grep CLIBS|cut -d= -f2-) \
 $(shell mbuild -f ${MATLAB}/bin/engopts.sh -v 2> /dev/null|grep LDFLAGS|cut -d= -f2-)
MATLAB_LDFLAGS = $(MATLAB_RAW_LDFLAGS:-lstdc++=)

ZIP_CFLAGS = $(shell pkg-config libzip --cflags)
ZIP_LDFLAGS = $(shell pkg-config libzip --libs)

OPENCV = -I ~/darpa-collaboration/include/`architecture-path`/ \
	-L ~/darpa-collaboration/lib/`architecture-path`/ \
	-I. $(shell pkg-config --define-variable=prefix=${HOME}/darpa-collaboration/lib/`architecture-path` --cflags --libs opencv)
OPENCV_I = -I${HOME}/darpa-collaboration/include/`architecture-path`/

CUKLT_CFLAGS = 
CUKLT_LDFLAGS = -lcudaklt

FLOWLIB_CFLAGS = -DCUDA_VERSION_32 -I${HOME}/darpa-collaboration/include/`architecture-path`/ -I${HOME}/darpa-collaboration/include/`architecture-path`/iu
FLOWLIB_LDFLAGS = -L ~/darpa-collaboration/lib/`architecture-path`/ -lflow -liucore -liuio

ifneq (,$(findstring -DUSE_IROBOT_FELZ,$(OPTIONS)))
IROBOTDIR = ${HOME}/darpa-collaboration/pkg/irobot_libcudafelz_1.2-roi
IROBOT_CFLAGS = -I $(IROBOTDIR)
IROBOT_LDFLAGS = -L $(IROBOTDIR) -lcudafelz
endif

LD_Q =  -L~/lib/$(QARCHITECTURE_PATH)/ \
	~/lib/$(QARCHITECTURE_PATH)/QobiScheme-AD.a  \
	~/lib/$(QARCHITECTURE_PATH)/scxl.a -lX11 -lXext -lz -lgmp

FFMPEG_LDFLAGS = -lavutil -lavformat -lavcodec -lz -lavutil -lm -lswscale

#DEBUG = -g3 -ggdb -gdwarf-2

C_SRC = \
	hmm-control.c \
	hmm-data.c \
	hmm-rand.c \
	hmm.c \
	idealib-c.c \
	pipeline-gui-c.c \
	toollib-c.c \
	toollib-hack-track-c.c \
	toollib-utils.c \
	toollib-v4l2-c.c \

CPP_SRC = \
    cupedro.cpp \
    idealib-cpp.cpp \

# All of these files must be prefixed with toollib-
SC_SRC = \
	cupedro-bindings.sc \
	hmm-train-classify.sc \
	hmm-wbm.sc \
	idealib-hash-table.sc \
	idealib-matplotlib.sc \
	idealib-pregexp.sc \
	idealib-tracks.sc \
	idealib-stuff.sc \
	toollib-c-bindings.sc \
	toollib-camera.sc \
	toollib-hack-track-drawable.sc \
	toollib-hack-track.sc \
	toollib-image-processing.sc \
	toollib-matlab.sc \
	toollib-misc.sc \

# removed because they don't compile

OBJ = $(SC_SRC:.sc=.o) $(C_SRC:.c=.o) $(CPP_SRC:.cpp=.o)
SCH = $(SC_SRC:.sc=.sch)
TARGET_SRC = $(TARGET:=.sc)
TARGET_SCH = $(TARGET_SRC:.sc=.sch)
CFILES = $(SC_SRC:.sc=.c)


.SECONDEXPANSION:

background_gmm: background_gmm.cpp
	$(CC) $(CFLAGS) $(OPECV) background_gmm.cpp -o background_gmm

${SCH}: $$(basename $$@).sc $$(SC_SRC) | in-port
	sch $(basename $@) $(SC_SRC)
	touch $(basename $@).sch

${TARGET_SCH}: $$(basename $$@).sc | in-port
	sch -main $(basename $@) $(SC_SRC)
	touch $(basename $@).sch

# -Ot -Ob -Og -g3
${TARGET}: $$(basename $$@).sc $$(basename $$@).sch $$(SCH) $(OBJ) $$@.o | environment
	scc-color -cc $(CC) $@.o $(SCCFLAGS) $(CFLAGS) $(DEBUG) $(FFMPEG_LDFLAGS) $(MATLAB_LDFLAGS) $(ZIP_CFLAGS) -g3 `imlib2-config --cflags --libs` -DGL_GLEXT_PROTOTYPES $(OBJ) -o $@ $(LD_Q) $(CUDA_LDFLAGS) $(CUDA_CFLAGS) $(IROBOT_CFLAGS) $(IROBOT_LDFLAGS) $(OPENCV) $(CUKLT_CFLAGS) $(CUKLT_LDFLAGS) $(ZIP_LDFLAGS) $(FLOWLIB_CFLAGS) $(FLOWLIB_LDFLAGS)

%.o: %.sc %.sch toollib-c-macros.sch | environment
	scc-color -cc $(CC) -sch 500 -scmh 800 $(SCCFLAGS) $(DEBUG) -I. -I ~/include $< -c $(CFLAGS) $(CUDA_CFLAGS) $(ZIP_CFLAGS) -g3 `imlib2-config --cflags` $(OPENCV_I)

%.o: %.c | environment
	$(CC) $(DEBUG) $(CFLAGS) -DGL_GLEXT_PROTOTYPES -I ~/lib/$(ARCHITECTURE_PATH) -I. $(MATLAB_CFLAGS) $(ZIP_CFLAGS) -std=gnu99 -c $< -g3 `imlib2-config --cflags` $(OPENCV_I) $(CUKLT_CFLAGS) $(FLOWLIB_CFLAGS) $(CUDA_CFLAGS)

%.o: %.cpp | environment
	g++ $(DEBUG) $(CFLAGS) -DGL_GLEXT_PROTOTYPES -I. -c $< -g3 `imlib2-config --cflags` $(CUDA_CFLAGS) $(IROBOT_CFLAGS) $(ZIP_CFLAGS) $(OPENCV_I) $(CUKLT_CFLAGS) $(FLOWLIB_CFLAGS) $(CUDA_CFLAGS)

environment:
	@if [ "${IROBOTDIR}" != "" ] && [ ! -d "${IROBOTDIR}" ]; then \
                echo "Failed to find irobot directory: ${IROBOTDIR}"; \
		echo "You must unpack ~abarbu/pkg/irobot_libcudafelz_1.2-roi-9999.tar.gz, to the above path."; \
		false; \
	fi;

	@if [ -z "${MATLAB}" ]; then \
		echo "Environment variables not set, prefix darpa-wrap to command"; \
		false; \
	fi;

in-port:
	@if [ ideas = `basename \`pwd\`` ]; then \
		echo "You cannot build ideas in the base directory, you must run make port and cd into the resulting directory first"; \
		false; \
	fi;

toollib-matlab.o: toollib-matlab.sc toollib-matlab.sch toollib-c-macros.sch toollib-matlab.h
	scc-color -cc $(CC) $(SCCFLAGS) $(CFLAGS) -I. -I ~/include -c toollib-matlab.sc $(MATLAB_CFLAGS) -std=gnu99
	rm toollib-matlab.c

xml-chicken: xml-chicken.sc
	csc xml-chicken.sc

tags: $(SC_SRC) $(C_SRC)
	etags -l c *.c -l scheme *.sc

# To add a new architecture, add architecture specific lines.
# The line prefix should be '#`architecture-path` ', see examples below.
port:
	-rm -rf $(ARCHITECTURE_PATH)
	mkdir $(ARCHITECTURE_PATH)
	cd $(ARCHITECTURE_PATH); csh -cf "ln -s ../*.{sc,c,cpp,h,S,sch} ."; csh -cf "ln -s ../cupedro/*.{c,cpp,h,hpp,sc,S,sch} ."
	$(MAKE) port-makefile

port-makefile:
	echo "# This file was generated automatically.  Do not edit." >\
		$(ARCHITECTURE_PATH)/makefile
	echo "# ----------------------------------------------------" >>\
		$(ARCHITECTURE_PATH)/makefile
	echo ARCHITECTURE_PATH = $(ARCHITECTURE_PATH) >>\
		$(ARCHITECTURE_PATH)/makefile
	echo ERROR_OK = _no_error >> $(ARCHITECTURE_PATH)/makefile
	grep "^#$(ARCHITECTURE_PATH)" makefile | \
		sed "s/#$(ARCHITECTURE_PATH) //" >>\
			$(ARCHITECTURE_PATH)/makefile
	echo include ../makefile >> $(ARCHITECTURE_PATH)/makefile

error:
	@echo "Do 'make port' to create an architecture specific directory."
	@exit 1

error_no_error:

# Architecture specific variables
# -------------------------------
#x86_64-Linux-2.6.38-ARCH CC = gcc
#x86_64-Linux-2.6.38-ARCH CFLAGS = -O2 -DLINUX $(OPTIONS)
#x86_64-Linux-2.6.38-ARCH SCCFLAGS = -O2
#x86_64-Linux-2.6.38-ARCH SCCLDFLAGS =
#x86_64-Linux-2.6.38-ARCH LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Linux-2.6.38-ARCH       $(INSTALLLIBA)/scxl.a\
#x86_64-Linux-2.6.38-ARCH       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Linux-2.6.38-ARCH CUDA_CFLAGS =
#x86_64-Linux-2.6.38-ARCH CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart

#x86_64-Linux-2.6.34.7-56.fc13.x86_64 CC = gcc
#x86_64-Linux-2.6.34.7-56.fc13.x86_64 CFLAGS = -O2 -DLINUX $(OPTIONS)
#x86_64-Linux-2.6.34.7-56.fc13.x86_64 SCCFLAGS = -O2
#x86_64-Linux-2.6.34.7-56.fc13.x86_64 SCCLDFLAGS =
#x86_64-Linux-2.6.34.7-56.fc13.x86_64 LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Linux-2.6.34.7-56.fc13.x86_64       $(INSTALLLIBA)/scxl.a\
#x86_64-Linux-2.6.34.7-56.fc13.x86_64       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Linux-2.6.34.7-56.fc13.x86_64 CUDA_CFLAGS =
#x86_64-Linux-2.6.34.7-56.fc13.x86_64 CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart

#x86_64-Gentoo-2.0.3 CC = clang -Wall
#x86_64-Gentoo-2.0.3 CFLAGS = -O2 -DLINUX $(OPTIONS)
#x86_64-Gentoo-2.0.3 SCCFLAGS = -O2
#x86_64-Gentoo-2.0.3 SCCLDFLAGS =
#x86_64-Gentoo-2.0.3 LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Gentoo-2.0.3       $(INSTALLLIBA)/scxl.a\
#x86_64-Gentoo-2.0.3       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Gentoo-2.0.3 CUDA_CFLAGS =
#x86_64-Gentoo-2.0.3 CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart

#i686-Debian-5.0.3 CC = gcc
#i686-Debian-5.0.3 CFLAGS = -O2 -DLINUX $(OPTIONS)
#i686-Debian-5.0.3 SCCFLAGS = -O2
#i686-Debian-5.0.3 SCCLDFLAGS =
#i686-Debian-5.0.3 LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#i686-Debian-5.0.3       $(INSTALLLIBA)/scxl.a\
#i686-Debian-5.0.3       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#i686-Debian-5.0.3 CUDA_CFLAGS =
#i686-Debian-5.0.3 CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart

#x86_64-Debian-5.0.6 CC = gcc
#x86_64-Debian-5.0.6 CFLAGS = -O2 -DLINUX $(OPTIONS)
#x86_64-Debian-5.0.6 SCCFLAGS = -O2
#x86_64-Debian-5.0.6 SCCLDFLAGS =
#x86_64-Debian-5.0.6 LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Debian-5.0.6       $(INSTALLLIBA)/scxl.a\
#x86_64-Debian-5.0.6       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Debian-5.0.6 CUDA_CFLAGS = -I/usr/local/include/ -I/usr/local/NVIDIA_GPU_Computing_SDK/C/common/inc
#x86_64-Debian-5.0.6 CUDA_LDFLAGS = -L/opt/cuda/lib64 -lcuda -lcudart

#x86_64-Debian-6.0.2 CC = gcc
#x86_64-Debian-6.0.2 CFLAGS = -O2 -DLINUX $(OPTIONS)
#x86_64-Debian-6.0.2 SCCFLAGS = -O2
#x86_64-Debian-6.0.2 SCCLDFLAGS =
#x86_64-Debian-6.0.2 LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Debian-6.0.2       $(INSTALLLIBA)/scxl.a\
#x86_64-Debian-6.0.2       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Debian-6.0.2 CUDA_CFLAGS = -I/usr/local/cuda/include/ -I/usr/local/NVIDIA_GPU_Computing_SDK/C/common/inc
#x86_64-Debian-6.0.2 CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart

#x86_64-Debian CC = gcc
#x86_64-Debian CFLAGS = -O2 -DLINUX $(OPTIONS)
#x86_64-Debian SCCFLAGS = -O2
#x86_64-Debian SCCLDFLAGS =
#x86_64-Debian LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Debian       $(INSTALLLIBA)/scxl.a\
#x86_64-Debian       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Debian CUDA_CFLAGS = -I/usr/local/cuda/include/ -I/usr/local/NVIDIA_GPU_Computing_SDK/C/common/inc
#x86_64-Debian CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart

#x86_64-Linux-2.6.32-5-amd64 CC = gcc
#x86_64-Linux-2.6.32-5-amd64 CFLAGS = -O2 -DLINUX $(OPTIONS)
#x86_64-Linux-2.6.32-5-amd64 SCCFLAGS = -O2
#x86_64-Linux-2.6.32-5-amd64 SCCLDFLAGS =
#x86_64-Linux-2.6.32-5-amd64 LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Linux-2.6.32-5-amd64       $(INSTALLLIBA)/scxl.a\
#x86_64-Linux-2.6.32-5-amd64       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Linux-2.6.32-5-amd64 CUDA_CFLAGS = -I/usr/local/cuda/include/ -I/usr/local/NVIDIA_GPU_Computing_SDK/C/common/inc
#x86_64-Linux-2.6.32-5-amd64 CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart

#x86_64-Gentoo-2.0.3-bgc CC = clang -Wall -g3 -ggdb -gdwarf-2
#x86_64-Gentoo-2.0.3-bgc CFLAGS = -O0 -DLINUX $(OPTIONS)
#x86_64-Gentoo-2.0.3-bgc SCCFLAGS = -O0
#x86_64-Gentoo-2.0.3-bgc SCCLDFLAGS =
#x86_64-Gentoo-2.0.3-bgc LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#x86_64-Gentoo-2.0.3-bgc      $(INSTALLLIBA)/scxl.a\
#x86_64-Gentoo-2.0.3-bgc       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#x86_64-Gentoo-2.0.3-bgc CUDA_CFLAGS = -I/opt/cuda/include/ -I/opt/cuda/sdk/C/common/inc/
#x86_64-Gentoo-2.0.3-bgc CUDA_LDFLAGS = -L/opt/cuda/lib64 -lcuda -lcudart

#i686-Ubuntu-9.10 CC = gcc
#i686-Ubuntu-9.10 CFLAGS = -O2 -DLINUX $(OPTIONS)
#i686-Ubuntu-9.10 SCCFLAGS = -O2
#i686-Ubuntu-9.10 SCCLDFLAGS =
#i686-Ubuntu-9.10 LRL = $(INSTALLLIBA)/QobiScheme-AD.a\
#i686-Ubuntu-9.10       $(INSTALLLIBA)/scxl.a\
#i686-Ubuntu-9.10       idealib-*.o -L/usr/X11R6/lib -lX11 -lm
#i686-Ubuntu-9.10 CUDA_CFLAGS = -I/usr/local/cuda/include/ -I/usr/local/NVIDIA_GPU_Computing_SDK/C/common/inc
#i686-Ubuntu-9.10 CUDA_LDFLAGS = -L/usr/local/cuda/lib64 -lcuda -lcudart


install: all
	cp $(TARGETS) $(INSTALLBINA)/.

uninstall:
	cd $(INSTALLBINA); rm -rf $(TARGETS)

.PHONY: clean real-clean environment
clean:
	-rm -f $(OBJ) $(TARGET_SRC:.sc=.c) $(TARGET_SRC:.sc=.o) \
	$(TARGET_SCH) core $(CFILES) $(SCH)

real-clean: clean
	-rm -r -f $(TARGET_SRC:.sc=)

# Automatic dependencies, don't modify them
