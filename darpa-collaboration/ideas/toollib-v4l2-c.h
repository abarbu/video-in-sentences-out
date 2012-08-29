/* LaHaShem HaAretz U'Mloah */
/* $Id: toollib-v4l2-c.h,v 1.6 2010-10-25 02:58:42 abarbu Exp $ */

#ifndef _V4L2_C_H
#define _V4L2_C_H

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
#include <linux/videodev2.h>
#include <stdbool.h>

#define NCAMERAS 4

struct camera_buffer {
  void *start;
  size_t length;
};

extern char* v4l2_video_device[NCAMERAS];
extern int v4l2_fd[NCAMERAS];
extern int v4l2_width[NCAMERAS], v4l2_height[NCAMERAS], v4l2_pixformat[NCAMERAS];
extern unsigned char *v4l2_frame[NCAMERAS], *v4l2_converted_frame[NCAMERAS];
extern struct camera_buffer *v4l2_buffers[NCAMERAS];
extern struct v4l2_buffer v4l2_frame_buf[NCAMERAS];

int  clamp(int v, int l, int u);
void errno_exit(const char *s);
int  xioctl(int fd, int request, void *arg);
void print_backtrace();

/*void yuvToBGRpixel(int y, int u, int v, unsigned char *b, unsigned char *g,
  unsigned char *r);*/
int yuyv_to_bgr32(const unsigned char *src, const int w, const int h,
		  unsigned char *dst, unsigned char* temp);
int yuv420_to_bgr32(const unsigned char *src, const int w, const int h,
		    unsigned char *dst, unsigned char* temp);
int yuv422_to_bgr32(const unsigned char *src, const int w, const int h,
		    unsigned char *dst, unsigned char* temp);
int mjpeg_to_bgr32(const unsigned char *src, const int w, const int h,
		   unsigned char *dst, unsigned char* temp);

int  v4l2_available(int camera);
void v4l2_close_video(int camera);
int  v4l2_frame_available(int camera);
int  v4l2_get_control(int camera, int id);
int  v4l2_get_frame(int camera);
void v4l2_initialize();
void v4l2_deinitialize();
void v4l2_initialize_mmap(int camera);
int v4l2_initialize_video(int camera, int pixformat, int width, int height);
void v4l2_interrupt_off(void);
void v4l2_interrupt_on(int frame_rate);
void v4l2_open_video(int camera);
int  v4l2_set_control(int camera, int id, int value);
void v4l2_set_video_device(int camera, char *device);
void v4l2_start_capturing(int camera);
void v4l2_stop_capturing(int camera);
void v4l2_uninitialize_mmap(int camera);
void v4l2_uninitialize_video(int camera);
int  v4l2_wait_for_frame(int camera);
void v4l2_write_ppm(int camera, char *name);

const char* v4l2_ioctl_request_code_to_string(long request);
const char* v4l2_cid_to_string(int cid);
void v4l2_print_available_controls();

#endif

/* Tam V'Nishlam Shevah L'El Borei Olam */
