/* LaHaShem HaAretz U'Mloah */

#ifndef _TOOL1LIB_HACK_TRACK_C_H
#define _TOOL1LIB_HACK_TRACK_C_H

#include <stdarg.h>
#include <math.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sched.h>
#include <signal.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/extensions/XShm.h>
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/mman.h>
#include <linux/videodev.h>
#include "toollib-c.h"
#include "toollib-v4l2-c.h"

#ifndef PI
#define PI 3.14159265358979323846
#endif

#ifndef MIN
#define MIN(a,b) ((a)>(b)?(b):(a))
#endif

#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif

#define MACH_EPS 1e-7
#define MAX_FRAMES 5400
#define MAX_OBJECTS 10
#define MAX_VERTICES 300

#define IS_ZERO(x) (-MACH_EPS<(x)&&(x)<MACH_EPS)
#define DEG_TO_RAD(x) (PI/180.0*(x))


#define HACK_TRACK_INVALID   0
#define HACK_TRACK_ARC       1
#define HACK_TRACK_LINE      2
#define HACK_TRACK_RECTANGLE 3

struct hack_track_drawable_tag
{
  int tag;
};

struct hack_track_arc
{
  int tag;
  Drawable d;
  GC gc;
  int x, y;
  unsigned int width,  height;
  int angle1, angle2;
};

struct hack_track_line
{
  int tag;
  Drawable d;
  GC gc;
  int x1, y1, x2, y2;
};

struct hack_track_rectangle
{
  int tag;
  Drawable d;
  GC gc;
  int x, y;
  unsigned int width, height;
};

#define HACK_TRACK_MAX_DRAWABLES 100

extern struct hack_track_drawable_tag
  *hack_track_drawables[HACK_TRACK_MAX_DRAWABLES];

int hack_track_nth_drawable_type(int n);
struct hack_track_arc* hack_track_nth_arc(int n);
struct hack_track_rectangle* hack_track_nth_rectangle(int n);
struct hack_track_line* hack_track_nth_line(int n);
void hack_track_free_nth(int n);
void hack_track_allocate_nth(int n, int tag);

/* Global Variables */

extern int hack_track_expose_handled;
extern int gfirst_frame[NCAMERAS], glast_frame[NCAMERAS];
extern int gobject_k[NCAMERAS][MAX_FRAMES];
extern float gellipse_x[NCAMERAS][MAX_FRAMES][MAX_OBJECTS];
extern float gellipse_y[NCAMERAS][MAX_FRAMES][MAX_OBJECTS];
extern float gellipse_theta[NCAMERAS][MAX_FRAMES][MAX_OBJECTS];
extern float gellipse_r[NCAMERAS][MAX_FRAMES][MAX_OBJECTS];
extern float gellipse_s[NCAMERAS][MAX_FRAMES][MAX_OBJECTS];
extern int hack_track_using_v4l[NCAMERAS];
extern int hack_track_width[NCAMERAS], hack_track_height[NCAMERAS];
extern unsigned char *hack_track_buffer[NCAMERAS];
extern int hack_track_real_time;
extern int hack_track_uid;
extern int hack_track_subsample[NCAMERAS];
extern int hack_track_capturing[NCAMERAS];
extern int hack_track_tracking_ellipses[NCAMERAS];
extern int hack_track_tracking_polygons[NCAMERAS];
extern int hack_track_hull[NCAMERAS];
extern Display *hack_track_display[NCAMERAS];
extern int hack_track_screen[NCAMERAS];
extern Window hack_track_window[NCAMERAS];
extern int hack_track_window_width[NCAMERAS];
extern int  hack_track_window_height[NCAMERAS];
extern int hack_track_i[NCAMERAS], hack_track_n[NCAMERAS];
extern int  hack_track_j[NCAMERAS], hack_track_m[NCAMERAS];
extern int hack_track_k[NCAMERAS], hack_track_l[NCAMERAS];
extern GC hack_track_gc[NCAMERAS];
extern XShmSegmentInfo hack_track_shminfo[NCAMERAS];
extern XImage *hack_track_shmimage[NCAMERAS];
extern unsigned char *hack_track_frames[NCAMERAS];
extern unsigned char *hack_track_code_frames[NCAMERAS];
extern unsigned char *hack_track_last_frame[NCAMERAS];
extern unsigned char **hack_track_last_code[NCAMERAS];
extern unsigned char **hack_track_conjure_code[NCAMERAS];
extern int hack_track_starting[NCAMERAS], hack_track_conjuring[NCAMERAS];
extern int hack_track_motion_threshold[NCAMERAS];
extern int hack_track_colour_pixel_mass_threshold[NCAMERAS];
extern int hack_track_colour_proximity_threshold[NCAMERAS];
extern int hack_track_motion_pixel_mass_threshold[NCAMERAS];
extern int hack_track_motion_proximity_threshold[NCAMERAS];
extern int hack_track_rhl[NCAMERAS], hack_track_rhh[NCAMERAS];
extern int hack_track_rsl[NCAMERAS], hack_track_rsh[NCAMERAS];
extern int hack_track_rvl[NCAMERAS], hack_track_rvh[NCAMERAS];
extern int hack_track_ghl[NCAMERAS], hack_track_ghh[NCAMERAS];
extern int hack_track_gsl[NCAMERAS], hack_track_gsh[NCAMERAS];
extern int hack_track_gvl[NCAMERAS], hack_track_gvh[NCAMERAS];
extern int hack_track_bhl[NCAMERAS], hack_track_bhh[NCAMERAS];
extern int hack_track_bsl[NCAMERAS], hack_track_bsh[NCAMERAS];
extern int hack_track_bvl[NCAMERAS], hack_track_bvh[NCAMERAS];
extern int hack_track_yhl[NCAMERAS], hack_track_yhh[NCAMERAS];
extern int hack_track_ysl[NCAMERAS], hack_track_ysh[NCAMERAS];
extern int hack_track_yvl[NCAMERAS], hack_track_yvh[NCAMERAS];
extern int hack_track_first_frame[NCAMERAS];
extern int hack_track_last_moving_valid[NCAMERAS];
extern int **hack_track_last_moving[NCAMERAS];
extern int hack_track_interrupt;
extern unsigned short hack_track_polygon_l[NCAMERAS][MAX_FRAMES][MAX_OBJECTS];
extern unsigned short hack_track_polygon_x
[NCAMERAS][MAX_FRAMES][MAX_OBJECTS][MAX_VERTICES];
extern unsigned short hack_track_polygon_y
[NCAMERAS][MAX_FRAMES][MAX_OBJECTS][MAX_VERTICES];
extern int hack_track_motion[NCAMERAS][MAX_FRAMES];
extern unsigned char hack_track_object_colour
[NCAMERAS][MAX_FRAMES][MAX_OBJECTS];

/* Function Prototypes */

void panic(const char *error_text, ...);
int get_first_frame(int camera);
int get_last_frame(int camera);
int get_object_k(int camera, int frame);
float get_ellipse_x(int camera, int frame, int k);
float get_ellipse_y(int camera, int frame, int k);
float get_ellipse_theta(int camera, int frame, int k);
float get_ellipse_r(int camera, int frame, int k);
float get_ellipse_s(int camera, int frame, int k);
float my_atan2(float x, float y);
float quadratic1(float a, float b, float c);
float quadratic2(float a, float b, float c);
float eigenvalue1(float xx, float xy, float yx, float yy);
float eigenvalue2(float xx, float xy, float yx, float yy);
float eigenvector_angle1(float xx, float xy, float yx, float yy);
float eigenvector_angle2(float xx, float xy, float yx, float yy);
int hack_track_get_current_frame(int camera);
int hack_track_get_hack_track_i(int camera);
int hack_track_angle_between(int l, int a, int h);
void hack_track_tracker(int camera,
			unsigned char *buffer,
			int frame,
			int height,
			int width,
			int capturing,
			int capturing_code,
			int tracking_ellipses,
			int tracking_polygons,
			int hull);
int hack_track_interrupts(void);
void hack_track_set_interrupts(int on);
void hack_track_handler(int signum);
void hack_track_draw_ellipse(int camera,
			     float x0,
			     float y0,
			     float t0,
			     float a,
			     float b);
void hack_track_set_real_time(void);
void hack_track_set_nonreal_time(void);
int hack_track_get_red(int camera, int i, int y, int x);
int hack_track_get_green(int camera, int i, int y, int x);
int hack_track_get_blue(int camera, int i, int y, int x);
int hack_track_get_pixel(int camera, int i, int y, int x);
void hack_track_set_red(int camera, int i, int y, int x, int v);
void hack_track_set_green(int camera, int i, int y, int x, int v);
void hack_track_set_blue(int camera, int i, int y, int x, int v);
void hack_track_set_pixel(int camera, int i, int y, int x, int v);
void hack_track_copy_frame_to_ximage(int camera, int i, XImage *ximage);
void hack_track_start(int camera, int frame);
void hack_track_finish(int camera);
void hack_track_blit_frame(int camera, int i);
void hack_track_blit_drawables(int camera);
void hack_track_free(int camera);
void hack_track_attach_viewfinder(int camera,
				  Display *display,
				  int screen,
				  Window window,
				  int window_x,
				  int window_y,
				  int window_width,
				  int window_height);
void hack_track_detach_viewfinder(int camera);
void hack_track_sigaction();
void hack_track_block(void);
void hack_track_unblock(void);
void hack_track_turn_on_viewfinder(int camera, int m);
int hack_track_get_max_frames();
void hack_track_turn_on_capturing(int camera, int i, int l, int n);
void hack_track_turn_off(int camera);
void hack_track_capture_frames(int camera, int i, int l, int n);
void hack_track_malloc(int camera, int nframes);
void hack_track_startup(int camera,
			int height,
			int width,
			int nframes,
			int subsample,
			int raw_frame_rate);
void hack_track_startup_offline(int camera,
				int height,
				int width,
				int nframes,
				int subsample);
void hack_track_startup_with_viewfinder(int camera,
					int height,
					int width,
					int nframes,
					int subsample,
					Display *display,
					int screen,
					Window window,
					int window_x,
					int window_y,
					int window_width,
					int window_height,
					int raw_frame_rate);
void hack_track_restartup(int camera,
			  int height,
			  int width,
			  int nframes,
			  int subsample,
			  int raw_frame_rate);
void hack_track_page_in_frames(int camera, int iframes, int nframes);
void hack_track_shutdown(int camera);
void hack_track_shutdown_offline(int camera);
void hack_track_send_frame(int camera, FILE *f, int i);
void hack_track_receive_frame(int camera, FILE *f, int i);
void hack_track_handler_multiple(int signum);
void hack_track_blit_buffer(int camera, const unsigned char *buffer, int width,
			    int height, int bpp, Display* dpy);
void hack_track_sigaction_multiple(int ncameras);
void hack_track_show_frame(int camera, int i);

void hack_track_exposed();
void hack_track_allocate_nth_drawable(int n, int tag);
void hack_track_free_nth_drawable(int n);
void* hack_track_frame_to_imlib(int camera, int frame_no);
#endif

/* Tam V'Nishlam Shevah L'El Borei Olam */
