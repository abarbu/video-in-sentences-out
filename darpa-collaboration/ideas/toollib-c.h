/* LaHaShem HaAretz U'Mloah */

#ifndef _TOOL1LIB_C_H
#define _TOOL1LIB_C_H

/* needs work: many of these are no longer needed */
#include <math.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <fcntl.h>
#include <string.h>
#include <memory.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <sched.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/fcntl.h>
#include <sys/signal.h>
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <linux/soundcard.h>

/* Global Variables */

extern struct xvimage *vect;

/* Function Prototypes */

int *make_c_int_vector(int l);
int c_int_vector_ref(int *v, int x);
void c_int_vector_set(int *v, int x, int i);
void free_c_int_vector(int *v);
int **make_c_int_matrix(int h, int w);
int c_int_matrix_ref(int **m, int y, int x);
void c_int_matrix_set(int **m, int y, int x, int i);
void free_c_int_matrix(int **m, int h);
struct xvimage *create_image(unsigned int height, unsigned int width);
void viff_set_pixel(struct xvimage *viff,
		    unsigned int y, unsigned int x, unsigned int grey);
void detect_edges(float a1, float a2, int t1, int t2, int l, int w,
		  struct xvimage *viff);
void detect_line_segments(float a1, float a2, int t1, int t2, int l, int w,
			  struct xvimage *viff);
int edge_pixel(struct xvimage *viff, int y, int x);
int number_of_line_segments(void);
int line_segment_x1(int i);
int line_segment_y1(int i);
int line_segment_x2(int i);
int line_segment_y2(int i);
void free_xvimage(struct xvimage *viff);
int wall_clock(void);

void panic(const char *error_text, ...);

#endif

/* Tam V'Nishlam Shevah L'El Borei Olam */
