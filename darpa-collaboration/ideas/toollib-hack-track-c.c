/* LaHaShem HaAretz U'Mloah */

#include <stdbool.h>
#include <X11/Xutil.h>
#include <Imlib2.h>
#include "toollib-hack-track-c.h"
#include "toollib-v4l2-c.h"

#define SHOW(x) printf("%s: %s %d\n", __func__, #x, x);
#define SHOWH(x) printf("%s: %s %x\n", __func__, #x, x);

/* Global Variables */

int hack_track_expose_handled;
int gobject_k[NCAMERAS][MAX_FRAMES];
int hack_track_height[NCAMERAS], hack_track_width[NCAMERAS];
int hack_track_interrupt_on[NCAMERAS];
int hack_track_exposing[NCAMERAS];
int hack_track_camera_on[NCAMERAS];
unsigned char *hack_track_buffer[NCAMERAS];
// to enable in initialize
int hack_track_capturing[NCAMERAS];// = {false, false, false, false};
Display *hack_track_display[NCAMERAS];
Display *hack_track_original_display[NCAMERAS];
int hack_track_screen[NCAMERAS];
Window hack_track_window[NCAMERAS];
int hack_track_viewer_width[NCAMERAS], hack_track_viewer_height[NCAMERAS];
int hack_track_viewer_x[NCAMERAS], hack_track_viewer_y[NCAMERAS];
/* hack_track_m - is this camera on? */
/* hack_track_l - fps divider itself */
/* hack_track_j - counter for fps divider for viewfinder */
/* hack_track_k - counter for fps divider for capturing */
/* hack_track_i - current frame being captured, reset when capturing a
   new sequence of frames */
/* hack_Track_n - number of frames to capture */
int hack_track_i[NCAMERAS], hack_track_n[NCAMERAS];
int hack_track_j[NCAMERAS], hack_track_m[NCAMERAS] = {0, 0, 0, 0};
int hack_track_k[NCAMERAS], hack_track_l[NCAMERAS] = {0, 0, 0, 0};
GC hack_track_gc[NCAMERAS];
XShmSegmentInfo hack_track_shminfo[NCAMERAS];
XImage *hack_track_shmimage[NCAMERAS];
unsigned char *hack_track_frames[NCAMERAS];
unsigned char *hack_track_last_frame[NCAMERAS];
int hack_track_starting[NCAMERAS];
int hack_track_first_frame[NCAMERAS];
int hack_track_interrupt = true;

#ifdef QOBISCHEME_XFLUSH
extern int safe_xflush;
#endif

struct hack_track_drawable_tag
  *hack_track_drawables[HACK_TRACK_MAX_DRAWABLES];

#ifdef V4L2_S2C_BACKTRACES
extern char *scrt5_debug_2doutput_2dport_v;
#endif

/* Function Definitions */
void panic(const char *error_text, ...)
{ va_list ap;
  char error_message[1024];
  va_start(ap, error_text);
  vsprintf(error_message, error_text, ap);
  fprintf(stderr, "panic: %s\n", error_message);
#ifdef V4L2_S2C_BACKTRACES
    scdebug_dobacktrace(10, 10, 80, scrt5_debug_2doutput_2dport_v);
#endif
  exit(EXIT_FAILURE);}

int hack_track_get_current_frame(int camera) {return hack_track_i[camera]-1;}

int hack_track_get_hack_track_i(int camera) { return hack_track_i[camera]; }

int hack_track_interrupts(void) {return hack_track_interrupt;}

void hack_track_set_interrupts(int on) {hack_track_interrupt = on;}

void hack_track_resize_image(const uint8_t *source, int source_w, int source_h,
			     int source_bpp, uint8_t *dest, int dest_w,
			     int dest_h, int dest_bpp)
{
  if(source_w == dest_w && source_h == dest_h && source_bpp == dest_bpp)
  { memcpy(dest, source, source_w*source_h*(source_bpp/8));
    return; }

  float w_stride = (float)source_w/dest_w, h_stride = (float)source_h/dest_h;
  if(floor(w_stride) != w_stride || w_stride != h_stride)
    panic("hack_track: source & destination height/width ratios must be equal integral multiples, instead: %f/%f | (%d,%d) -> (%d,%d)\n",
	  w_stride, h_stride, source_w, source_h, dest_w, dest_h);

  int stride = floor(w_stride) - 1;
  const uint8_t *s = source;
  uint8_t *d = dest;

  if(24 == source_bpp && 24 == dest_bpp) {
    const uint8_t *end = source+source_w*source_h*3;
    while(s < end)
    {
      const uint8_t* row = s+3*source_w;
      while(s<row)
      {
	*d++ = *s++;
	*d++ = *s++;
	*d++ = *s++;
	s += 3*stride;
      }
      s += 3*source_w*stride;
    }
  } else if(24 == source_bpp && 32 == dest_bpp) {
    const uint8_t *end = source+source_w*source_h*3;
    while(s < end)
    {
      const uint8_t* row = s+3*source_w;
      while(s<row)
      {
	*d++ = *s++;
	*d++ = *s++;
	*d++ = *s++;
	*d++ = 0;
	s += 3*stride;
      }
      s += 3*source_w*stride;
    }
  } else if(32 == source_bpp && 24 == dest_bpp) {
    const uint8_t *end = source+source_w*source_h*4;
    while(s < end)
    {
      const uint8_t* row = s+4*source_w;
      while(s<row)
      {
	*d++ = *s++;
	*d++ = *s++;
	*d++ = *s++;
	s++;
	s += 4*stride;
      }
      s += 4*source_w*stride;
    }
  } else if(32 == source_bpp && 32 == dest_bpp) {
    const uint8_t *end = source+source_w*source_h*4;
    while(s < end)
    {
      const uint8_t* row = s+4*source_w;
      while(s<row)
      {
	*d++ = *s++;
	*d++ = *s++;
	*d++ = *s++;
	*d++ = *s++;
	s += 4*stride;
      }
      s += 4*source_w*stride;
    }
  }
}

int v4l2_discard_frame(int camera);

static int debug_handler = 0;
void debug_h(int lineno) {
  if(debug_handler) {
    fprintf(stdout, "debug_h #%d\n", lineno);
  }
}

void hack_track_handle_one(int camera, int signum)
{
  if (!hack_track_interrupt) {
    debug_h(__LINE__);
    return;
  }
  if (!v4l2_frame_available(camera)) {
    debug_h(__LINE__);
    return;
  }

  if (hack_track_display[camera] && (hack_track_m[camera]>0))
    hack_track_j[camera] = (hack_track_j[camera]+1)%hack_track_m[camera];
  if (hack_track_l[camera]>0)
    hack_track_k[camera] = (hack_track_k[camera]+1)%hack_track_l[camera];

  if (!(hack_track_j[camera]==0 || hack_track_k[camera]>0))
  {
    v4l2_discard_frame(camera);
    return;
  }

  if (v4l2_get_frame(camera)) return;

  unsigned char *buffer = hack_track_buffer[camera];
  /* hack_track_l[camera] is the capture downsample factor. If
     hack_track_l[camera]==0 then capturing is turned off. If
     hack_track_l[camera]!=0 then the capture rate is
     raw_frame_rate/hack_track_l[camera]. */
  if (hack_track_l[camera]>0)
  /* hack_track_k[camera] does the downsampling for capturing. It counts from
     0,...,hack_track_l[camera]-1 and cycles. Capturing is done when
     hack_track_k[camera]==0. */
  { if (hack_track_k[camera]==0)
      /* hack_track_i[camera] is the index of the current frame. It is used to
	 index into the saved frames as well as the saved tracking data. */
    { if (hack_track_i[camera]<hack_track_n[camera])
      {	if (hack_track_capturing[camera])
	{
	  unsigned char *p1, *p2, *pl;
	  for (p1 = buffer,
	  	 p2 = hack_track_frames[camera]+ hack_track_i[camera]*hack_track_width[camera]
	  	 *hack_track_height[camera]*3,
	  	 pl = &buffer[hack_track_height[camera]*hack_track_width[camera]*4];
	       p1<pl;)
	  { *p2++ = *p1++;
	    *p2++ = *p1++;
	    *p2++ = *p1++;
	    p1++;}
	  hack_track_i[camera]++;
	  if(hack_track_display[camera] && hack_track_expose_handled)
	  { hack_track_expose_handled = 0;
	    XExposeEvent e;
	    memset(&e,0,sizeof(e));
	    e.display = hack_track_display[camera];
	    e.type = Expose;
	    e.window = hack_track_window[camera];
	    XSendEvent(hack_track_display[camera],
		       hack_track_window[camera],
		       True, ExposureMask, (XEvent*)&e);
#ifdef QOBISCHEME_XFLUSH
	    if(safe_xflush)
#endif
	      XFlush(hack_track_display[camera]);
	  }}}
      /* Stop capturing when hack_track_i[camera]>=hack_track_n[camera]. */
      else hack_track_l[camera] = 0;}}
  /* hack_track_m[camera] is the viewfinder refresh downsample factor. If
     hack_track_m[camera]==0 then the viewfinder is turned off. If
     hack_track_m[camera]!=0
     then the viewfinder refresh rate is raw_frame_rate/hack_track_m[camera].
     */
  if (hack_track_display[camera] && (hack_track_m[camera]>0))
    /* hack_track_j[camera] does the downsampling for the viewfinder refresh. It
       counts from 0,...,hack_track_m[camera]-1 and cycles. Viewfinder refresh is
     done when hack_track_j[camera]==0. */
  { if (hack_track_j[camera]==0)
    { 	  
      hack_track_blit_buffer(camera, buffer, hack_track_width[camera],
			     hack_track_height[camera], 32,
			     hack_track_display[camera]);
      hack_track_blit_drawables(camera);
#ifdef QOBISCHEME_XFLUSH
      if(safe_xflush)
#endif
	XFlush(hack_track_display[camera]);
    }
  }
}

int hack_track_get_red(int camera, int i, int y, int x)
{ int p = 3*((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
  return hack_track_frames[camera][p+2];}

int hack_track_get_green(int camera, int i, int y, int x)
{ int p = 3*((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
  return hack_track_frames[camera][p+1];}

int hack_track_get_blue(int camera, int i, int y, int x)
{ int p = 3*((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
  return hack_track_frames[camera][p];}

int hack_track_get_pixel(int camera, int i, int y, int x)
{ int p = 3*
	  ((i*hack_track_height[camera]+y)*
	   hack_track_width[camera]+x);
  return 65536*hack_track_frames[camera][p+2]+
	   256*hack_track_frames[camera][p+1]+
	       hack_track_frames[camera][p];}

void hack_track_set_red(int camera, int i, int y, int x, int v)
{ int p = 3*((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
  hack_track_frames[camera][p+2] = v;}

void hack_track_set_green(int camera, int i, int y, int x, int v)
{ int p = 3*((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
  hack_track_frames[camera][p+1] = v;}

void hack_track_set_blue(int camera, int i, int y, int x, int v)
{ int p = 3*((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
  hack_track_frames[camera][p] = v;}

void hack_track_set_pixel(int camera, int i, int y, int x, int v)
{ int p = 3*((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
  hack_track_frames[camera][p+2] = v>>16;
  hack_track_frames[camera][p+1] = (v>>8)&255;
  hack_track_frames[camera][p] = v&255;}

void hack_track_copy_frame_to_ximage(int camera, int i, XImage *ximage)
{ int y, x, p;
  for (y = 0; y<hack_track_height[camera]; y++)
  { for (x = 0; x<hack_track_width[camera]; x++)
    { p = 3*
	  ((i*hack_track_height[camera]+y)*hack_track_width[camera]+x);
      XPutPixel(ximage, x, y,
		65536*hack_track_frames[camera][p+2]+
		256*hack_track_frames[camera][p+1]+
		hack_track_frames[camera][p]);}}}

void hack_track_start(int camera, int frame)
{ hack_track_first_frame[camera] = frame;}

void hack_track_finish(int camera) {}

void hack_track_blit_drawables(int camera)
{ int i;
  for(i = 0; i < HACK_TRACK_MAX_DRAWABLES; ++i)
  {
    if(hack_track_drawables[i])
    {
      switch(hack_track_drawables[i]->tag )
      {
      case HACK_TRACK_ARC:
	{
	  struct hack_track_arc *arc
	    = (struct hack_track_arc*)hack_track_drawables[i];
	  XDrawArc(hack_track_display[camera],
		   arc->d, arc->gc,
		   arc->x, arc->y,
		   arc->width, arc->height,
		   arc->angle1, arc->angle2);
	  break;
	}
      case HACK_TRACK_LINE:
	{
	  struct hack_track_line *line =
	    (struct hack_track_line*)hack_track_drawables[i];
	  XDrawLine(hack_track_display[camera],
		    line->d, line->gc,
		    line->x1, line->y1,
		    line->x2, line->y2);
	  break;
	}
      case HACK_TRACK_RECTANGLE:
	{
	  struct hack_track_rectangle *rectangle
	    = (struct hack_track_rectangle*)hack_track_drawables[i];
	  XDrawRectangle(hack_track_display[camera],
			 rectangle->d, rectangle->gc,
			 rectangle->x, rectangle->y,
			 rectangle->width, rectangle->height);
	  break;
	}
      default:
	panic("Bad display of hack track drawable %x@%d tag = %d\n",
	      hack_track_drawables[i], i, hack_track_drawables[i]->tag);
      }
    }
  }
}

void hack_track_show_frame(int camera, int i)
{ int y;
  hack_track_blit_buffer(camera,
			 &(hack_track_frames[camera]
			   [3*i*hack_track_height[camera]
			    *hack_track_width[camera]]),
			 hack_track_width[camera],
			 hack_track_height[camera],
			 24,
			 hack_track_original_display[camera]);
#ifdef QOBISCHEME_XFLUSH
  if(safe_xflush)
#endif
    XFlush(hack_track_original_display[camera]);
}

void hack_track_blit_buffer(int camera, const unsigned char *buffer,
			    int width, int height, int bpp,
			    Display* dpy)
{ hack_track_resize_image(buffer,
  			  width,
			  height,
			  bpp,
  			  hack_track_shmimage[camera]->data,
  			  hack_track_viewer_width[camera],
  			  hack_track_viewer_height[camera],
  			  hack_track_shmimage[camera]->bits_per_pixel);
  XShmPutImage(dpy,
  	       hack_track_window[camera],
  	       hack_track_gc[camera],
  	       hack_track_shmimage[camera],
  	       0,
  	       0,
  	       hack_track_viewer_x[camera],
  	       hack_track_viewer_y[camera],
  	       hack_track_viewer_width[camera],
  	       hack_track_viewer_height[camera],
  	       False);
}

void hack_track_free(int camera)
{ free(hack_track_frames[camera]);
  free(hack_track_last_frame[camera]);}

void hack_track_attach_viewfinder(int camera,
				  Display *display,
				  int screen,
				  Window window,
				  int viewer_x,
				  int viewer_y,
				  int viewer_width,
				  int viewer_height)
{ XWindowAttributes attributes;
  Visual *visual;
  int visualid;
  int depth;
  XGCValues gc_values;
  hack_track_display[camera] = XOpenDisplay(0);
  hack_track_original_display[camera] = display;
  hack_track_screen[camera] = screen;
  hack_track_window[camera] = window;
  hack_track_viewer_x[camera] = viewer_x;
  hack_track_viewer_y[camera] = viewer_y;
  hack_track_viewer_width[camera] = viewer_width;
  hack_track_viewer_height[camera] = viewer_height;
  XGetWindowAttributes(display, window, &attributes);
  visual = attributes.visual;
  depth = attributes.depth;
  if (depth!=24&&depth!=32) panic("unrecognized depth");
  visualid = attributes.visual->visualid;
  if (visualid!=32&&visualid!=33&&visualid!=34&&visualid!=35)
  { panic("unrecognized visual id");}
  gc_values.graphics_exposures = False;
  /* needs work: To check that succeeds */
  hack_track_gc[camera] =
    XCreateGC(display, window, GCGraphicsExposures, &gc_values);
  /* needs work: To check that succeeds */
  hack_track_shmimage[camera] =
    XShmCreateImage(display,
		    visual,
		    depth,
		    ZPixmap,
		    NULL,
		    &hack_track_shminfo[camera],
		    hack_track_viewer_width[camera],
		    hack_track_viewer_height[camera]);
  if (hack_track_shmimage[camera]->bits_per_pixel!=24&&
      hack_track_shmimage[camera]->bits_per_pixel!=32)
  { panic("unrecognized bits_per_pixel");}
  if ((hack_track_shminfo[camera].shmid =
       shmget(IPC_PRIVATE,
	      hack_track_viewer_width[camera]*hack_track_viewer_height[camera]*4,
	      IPC_CREAT|0777))==-1)
  { perror("hack_track shmget failed");
    exit(-1);}
  if ((hack_track_shminfo[camera].shmaddr =
       shmat(hack_track_shminfo[camera].shmid, (char *)0, 0))==
      (char *)(-1))
  { perror("hack_track shmat failed");
    exit(-1);}
  hack_track_shmimage[camera]->data = hack_track_shminfo[camera].shmaddr;
  /* needs work: To check that succeeds */
  XShmAttach(display, &hack_track_shminfo[camera]);}

void hack_track_detach_viewfinder(int camera)
/* needs work: To check that succeeds */
{ XClearArea(hack_track_display[camera],
	     hack_track_window[camera],
	     hack_track_viewer_x[camera],
	     hack_track_viewer_y[camera],
	     hack_track_width[camera],
	     hack_track_height[camera],
	     False);
  XShmDetach(hack_track_display[camera], &hack_track_shminfo[camera]);
  if (shmdt(hack_track_shminfo[camera].shmaddr)==-1)
  { perror("hack_track shmdt failed");
    exit(-1);}
  /* needs work: To undo shmget */
  /* needs work: To undo XShmCreateImage */
  /* needs work: To check that succeeds */
  XFreeGC(hack_track_display[camera], hack_track_gc[camera]);
  XDestroyImage(hack_track_shmimage[camera]);
  hack_track_shmimage[camera] = NULL;
  XCloseDisplay(hack_track_display[camera]);
  hack_track_display[camera] = NULL;
  hack_track_original_display[camera] = NULL;
  hack_track_screen[camera] = -1;
  hack_track_window[camera] = -1;
  hack_track_viewer_x[camera] = -1;
  hack_track_viewer_y[camera] = -1;
  hack_track_viewer_width[camera] = -1;
  hack_track_viewer_height[camera] = -1;}

void hack_track_handler(int signum)
{ int camera;
  for(camera = 0; camera < NCAMERAS; ++camera) {
    if(hack_track_camera_on[camera])
      hack_track_handle_one(camera, signum);
  }
}

void hack_track_sigaction()
{ struct sigaction sigact;
  sigact.sa_handler = hack_track_handler;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = SA_RESTART;
  if (sigaction(SIGALRM, &sigact, NULL))
    panic("hack_track sigaction failed %s", strerror(errno));}

void hack_track_block()
{ sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGALRM);
  sigprocmask(SIG_BLOCK, &set, NULL);}

void hack_track_unblock()
{ sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGALRM);
  sigprocmask(SIG_UNBLOCK, &set, NULL);}

void hack_track_turn_on_viewfinder(int camera, int m)
{ if (m<=0) panic("m must be positive");
  hack_track_unblock();
  hack_track_capturing[camera] = false;
  hack_track_l[camera] = 0;
  hack_track_m[camera] = m;}

int hack_track_get_max_frames()
{ return MAX_FRAMES; }

/** 
 * @param i The current frame being captured (reset for a new sequence of frames) 
 * @param l fps divider
 * @param n Total number of frames to capture
 */ 
void hack_track_turn_on_capturing(int camera, int i, int l, int n)
{ if (i < 0 || i >= MAX_FRAMES) panic("i out of bounds");
  if (l <= 0) panic("l must be positive");
  if (n <= 0 || n > MAX_FRAMES) panic("n out of bounds");
  hack_track_unblock();
  hack_track_capturing[camera] = true;
  hack_track_i[camera] = i;
  hack_track_j[camera] = 0;
  hack_track_k[camera] = 0;
  hack_track_n[camera] = n;
  hack_track_l[camera] = l;}

int hack_track_async_capture_progress(int camera)
{ return 0; }

void hack_track_turn_off(int camera)
{ hack_track_block();		/* needs work: may turn off other copy */
  hack_track_l[camera] = 0;
  hack_track_m[camera] = 0;
  hack_track_capturing[camera] = false;}

void hack_track_capture_frames(int camera, int i, int l, int n)
{ if (i<0||i>=MAX_FRAMES) panic("i out of bounds");
  if (l<=0) panic("l must be positive");
  if (n<=0||n>MAX_FRAMES) panic("n out of bounds");
  hack_track_unblock();
  hack_track_capturing[camera] = true;
  hack_track_i[camera] = i;
  hack_track_j[camera] = 0;
  hack_track_k[camera] = 0;
  hack_track_n[camera] = n;
  hack_track_l[camera] = l;
  while (hack_track_i[camera]<hack_track_n[camera]) pause();
  hack_track_block();
  hack_track_l[camera] = 0;
  hack_track_capturing[camera] = false;}

void hack_track_malloc(int camera, int nframes)
{ hack_track_frames[camera] =
  (unsigned char *)
  malloc(nframes*hack_track_height[camera]*
	 hack_track_width[camera]*3*sizeof(unsigned char));
  if (hack_track_frames[camera]==NULL)
    panic("hack_track malloc failed\n");
  hack_track_last_frame[camera] =
  (unsigned char *)
  malloc(hack_track_height[camera]*
	 hack_track_width[camera]*3*sizeof(unsigned char));
  if (hack_track_last_frame[camera]==NULL)
    panic("hack_track malloc failed\n");}

int nr_hack_track_cameras()
{
  int nr = 0, i;
  for(i = 0; i < NCAMERAS; ++i) if(hack_track_interrupt_on[i]) nr++;
  return nr;
}

void hack_track_interrupts_off(int camera)
{
  hack_track_interrupt_on[camera] = false;
  if(!nr_hack_track_cameras())
  { hack_track_interrupt = false;
    v4l2_interrupt_off();}}

void hack_track_interrupts_on(int camera, int frame_rate)
{
  hack_track_interrupt_on[camera] = true;
  hack_track_interrupt = true;
  v4l2_interrupt_on(frame_rate);
}

void hack_track_startup(int camera,
			int width,
			int height,
			int nframes,
			int subsample,
			int raw_frame_rate)
{ hack_track_startup_offline(camera, height, width, nframes, subsample);
  v4l2_open_video(camera);
  // andrei: removed since setting the frame rate is broken
  //  v4l_set_frame_rate(camera, raw_frame_rate, false);
  hack_track_sigaction();
  hack_track_malloc(camera, nframes);
  // Hardcoded for the logitech orbit AF
  // printf("ZAP-5\n");
  if(width < 1000 && height < 1000) 
    v4l2_initialize_video(camera, V4L2_PIX_FMT_MJPEG, height, width);
  else
    v4l2_initialize_video(camera, V4L2_PIX_FMT_YUYV, height, width);
  hack_track_buffer[camera] = v4l2_converted_frame[camera];
  hack_track_display[camera] = NULL;
  hack_track_original_display[camera] = NULL;
  v4l2_start_capturing(camera);
  hack_track_interrupts_on(camera, raw_frame_rate);
  hack_track_camera_on[camera] = true; }

void hack_track_startup_with_viewfinder(int camera,
					int width,
					int height,
					int nframes,
					int subsample,
					Display *display,
					int screen,
					Window window,
					int viewer_x,
					int viewer_y,
					int viewer_width,
					int viewer_height,
					int raw_frame_rate)
{ hack_track_startup(camera, height, width, nframes,
		     subsample, raw_frame_rate);
  hack_track_attach_viewfinder(camera,
			       display,
			       screen,
			       window,
			       viewer_x,
			       viewer_y,
			       viewer_width,
			       viewer_height);}

void hack_track_restartup(int camera,
			  int width,
			  int height,
			  int nframes,
			  int subsample,
			  int raw_frame_rate)
{ Display *display = hack_track_original_display[camera];
  int viewer_width = hack_track_viewer_width[camera];
  int viewer_height = hack_track_viewer_height[camera];
  int viewer_x = hack_track_viewer_x[camera];
  int viewer_y = hack_track_viewer_y[camera];
  Window window = hack_track_window[camera];
  int screen = hack_track_screen[camera];
  hack_track_shutdown(camera);
  if(display)
    hack_track_startup(camera, height, width, nframes,
		       subsample, raw_frame_rate);
  else
    hack_track_startup_with_viewfinder(camera, height, width, nframes,
				       subsample, display, screen, window,
				       viewer_x, viewer_y, viewer_width,
				       viewer_height, raw_frame_rate);}

void hack_track_page_in_frames(int camera, int iframes, int nframes)
{ int i = iframes*(hack_track_height[camera])*
	  hack_track_width[camera]*3*sizeof(unsigned char);
  int n = nframes*(hack_track_height[camera])*
	  hack_track_width[camera]*3*sizeof(unsigned char);
  for (; i<n; i++) hack_track_frames[camera][i]++; }

void hack_track_shutdown(int camera)
{ hack_track_l[camera] = 0;
  hack_track_m[camera] = 0;
  hack_track_interrupts_off(camera);
  if (hack_track_display[camera]!=NULL) hack_track_detach_viewfinder(camera);
  hack_track_camera_on[camera] = false;
  v4l2_close_video(camera);
  hack_track_buffer[camera] = NULL;
  hack_track_shutdown_offline(camera); }

void hack_track_send_frame(int camera, FILE *f, int i)
{ int amount = 3*(hack_track_height[camera])*
    hack_track_width[camera];
  int p = 3*(((i+1)*hack_track_height[camera]))*
	  hack_track_width[camera];
  while (amount>0)
  { amount -= write(fileno(f), &hack_track_frames[camera][p-amount], amount); }}

void hack_track_receive_frame(int camera, FILE *f, int i)
{ int amount = 3*(hack_track_height[camera])*
    hack_track_width[camera];
  char *p = &hack_track_frames[camera]
	     [3*((i*hack_track_height[camera]))*
	      hack_track_width[camera]];
  for (; amount>0; amount--) *(p++) = fgetc(f);}

void hack_track_startup_offline(int camera,
				int width,
				int height,
				int nframes,
				int subsample)
{ hack_track_height[camera] = height;
  hack_track_width[camera] = width;
  hack_track_malloc(camera, nframes);
  hack_track_display[camera] = NULL;
  hack_track_original_display[camera] = NULL;
  hack_track_starting[camera] = true;
  hack_track_camera_on[camera] = false;
  hack_track_expose_handled = 1;
  hack_track_i[camera] = 0;
  hack_track_j[camera] = 0;
  hack_track_k[camera] = 0;
  hack_track_n[camera] = 0;
  hack_track_l[camera] = 0;
  if(!v4l2_available(camera))
    panic("failed to start camera %d\n", camera);}

void hack_track_shutdown_offline(int camera)
{ hack_track_camera_on[camera] = false;
  hack_track_free(camera);}

void hack_track_exposed()
{
  hack_track_expose_handled = 1;
}

int hack_track_nth_drawable_type(int n)
{
  if(!hack_track_drawables[n])
    return 0;
  return hack_track_drawables[n]->tag;
}

struct hack_track_arc *hack_track_nth_arc(int n)
{
  return (struct hack_track_arc*) hack_track_drawables[n];
}

struct hack_track_rectangle *hack_track_nth_rectangle(int n)
{
  return (struct hack_track_rectangle*) hack_track_drawables[n];
}

struct hack_track_line *hack_track_nth_line(int n)
{
  return (struct hack_track_line*) hack_track_drawables[n];
}

void hack_track_free_nth_drawable(int n)
{
  free(hack_track_drawables[n]);
  hack_track_drawables[n] = NULL;
}

void hack_track_allocate_nth_drawable(int n, int tag)
{
  hack_track_free_nth_drawable(n);
  switch(tag)
  {
  case HACK_TRACK_ARC:
    hack_track_drawables[n] = malloc(sizeof(struct hack_track_arc));
    break;
  case HACK_TRACK_LINE:
    hack_track_drawables[n] = malloc(sizeof(struct hack_track_line));
    break;
  case HACK_TRACK_RECTANGLE:
    hack_track_drawables[n] = malloc(sizeof(struct hack_track_rectangle));
    break;
  default:
    panic("Bad allocate of hack track drawable %x@%d tag = %d\n",
	  hack_track_drawables[n], n, tag);
  }

  hack_track_drawables[n]->tag = tag;
}

void* hack_track_frame_to_imlib(int camera, int frame_no)
{ 
  // Image dimensions
  int h = hack_track_height[camera];
  int w = hack_track_width[camera];

  // Create the destinage image
  Imlib_Image res = imlib_create_image(w, h);
  imlib_context_set_image(res);

  // Convert from RGB formatted memory to ARGB formatted memory
  int size = h * w;
  const unsigned char* rgb_pixels = &hack_track_frames[camera][3 * frame_no * size];
  DATA32* argb_pixels = imlib_image_get_data();

  unsigned int pixel = 0;
  for(int i = 0; i < size; ++i) {
    pixel = ((unsigned int) *rgb_pixels++) << 0;   // B
    pixel += ((unsigned int) *rgb_pixels++) << 8;  // G 
    pixel += ((unsigned int) *rgb_pixels++) << 16; // R
    *argb_pixels++ = pixel;
  }
  
  // Return imlib image handle
  return res;
}




