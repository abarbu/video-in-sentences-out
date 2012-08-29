IMLIB2_CFLAGS = $(shell imlib2-config --cflags)
IMLIB2_LDFLAGS = $(shell imlib2-config --libs)
FFMPEG_LDFLAGS = -lavutil -lavformat -lavcodec -lz -lavutil -lm -lswscale

install: ffmpeg ffmpeg-bindings
	mkdir -p ../lib/`architecture-path`
	cp libffmpeg-bindings.so ../lib/`architecture-path`

ffmpeg: ffmpeg-matlab.c
	mex -v CC=gcc-4.4 CFLAGS='$$CFLAGS -Wall -std=c99 $(IMLIB2_CFLAGS)' LDFLAGS='$$LDFLAGS -Wall -std=c99 $(IMLIB2_LDFLAGS) $(FFMPEG_LDFLAGS)' ffmpeg-matlab.c -o ffmpeg

ffmpeg-bindings: ffmpeg-bindings.c
	gcc -Wall -Wextra -pedantic -o libffmpeg-bindings.so ffmpeg-bindings.c -I. -fPIC -shared -std=c99 $(IMLIB2_CFLAGS) $(IMLIB2_LDFLAGS) $(FFMPEG_LDFLAGS)
