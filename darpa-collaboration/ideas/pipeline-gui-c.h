#ifndef _PIPELINE_GUI_C_H
#define _PIPELINE_GUI_C_H

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <stdbool.h>


void pipeline_gui_startup_with_viewfinder(Display *display,
                                          int screen,
                                          Window window,
                                          int viewer_x,
                                          int viewer_y,
                                          int viewer_width,
                                          int viewer_height,
                                          int raw_frame_rate);
void pipeline_gui_shutdown();
#endif
