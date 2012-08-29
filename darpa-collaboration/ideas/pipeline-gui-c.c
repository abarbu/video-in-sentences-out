#include <stdbool.h>
#include <X11/Xutil.h>

#include <signal.h>
#include <stdarg.h>
#include <execinfo.h>
#include "pipeline-gui-c.h"

static int g_interrupt_frame_rate = 0;
static Display *g_pipeline_gui_display = NULL;
static Window g_pipeline_gui_window = 0;

static void 
panic (const char *error_text, ...)
{ 
  va_list ap;
  char error_message[1024];
  va_start(ap, error_text);
  vsprintf(error_message, error_text, ap);
  fprintf(stderr, "panic: %s\n", error_message);
  exit(EXIT_FAILURE);
}

static void 
pipeline_gui_interrupt_on (int frame_rate)
{ 
  if(g_interrupt_frame_rate &&
     g_interrupt_frame_rate < frame_rate)
    return;

  g_interrupt_frame_rate = frame_rate;
  struct itimerval itimerval;
  itimerval.it_interval.tv_sec = 0;
  itimerval.it_interval.tv_usec = 1000000/frame_rate;
  itimerval.it_value.tv_sec = 0;
  itimerval.it_value.tv_usec = 1000000/frame_rate;
  setitimer(ITIMER_REAL, &itimerval, (struct itimerval *)0);
}

static void 
pipeline_gui_interrupt_off ()
{ 
  g_interrupt_frame_rate = 0;
  struct itimerval itimerval;
  itimerval.it_interval.tv_sec = 0;
  itimerval.it_interval.tv_usec = 0;
  itimerval.it_value.tv_sec = 0;
  itimerval.it_value.tv_usec = 0;
  setitimer(ITIMER_REAL, &itimerval, (struct itimerval *)0);
}


static void 
pipeline_gui_handler (int signum)
{ 
  /*
   * Send out an expose event:
   */
  if (g_pipeline_gui_display && g_pipeline_gui_window)
    {
      XExposeEvent e;
      memset(&e,0,sizeof(e));
      e.display = g_pipeline_gui_display;
      e.type = Expose;
      e.window = g_pipeline_gui_window;
      XSendEvent(g_pipeline_gui_display,
                 g_pipeline_gui_window,
                 True, ExposureMask, (XEvent*)&e);
#ifdef QOBISCHEME_XFLUSH
      if(safe_xflush)
#endif
        XFlush(g_pipeline_gui_display);
    }
}


static void 
pipeline_gui_attach_viewfinder (Display *display,
                                int      screen,
                                Window   window,
                                int      viewer_x,
                                int      viewer_y,
                                int      viewer_width,
                                int      viewer_height)
{
  g_pipeline_gui_display = display;
  g_pipeline_gui_window = window;

  XWindowAttributes attributes;
  Visual *visual;
  int visualid;
  int depth;
  XGCValues gc_values;  

  XGetWindowAttributes(display, window, &attributes);
  visual = attributes.visual;
  depth = attributes.depth;
  if (depth!=24&&depth!=32) panic("unrecognized depth");
  visualid = attributes.visual->visualid;
  if (visualid!=32&&visualid!=33&&visualid!=34&&visualid!=35)
  { panic("unrecognized visual id");}
  gc_values.graphics_exposures = False;
  XCreateGC(display, window, GCGraphicsExposures, &gc_values);
}

static void 
signal_block ()
{ 
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGALRM);
  sigprocmask(SIG_BLOCK, &set, NULL);
}

static void 
signal_unblock ()
{ 
  sigset_t set;
  sigemptyset(&set);
  sigaddset(&set, SIGALRM);
  sigprocmask(SIG_UNBLOCK, &set, NULL);
}


static void 
pipeline_gui_sigaction ()
{ 
  fprintf (stderr, "setting up signal handler\n");
  struct sigaction sigact;
  sigact.sa_handler = pipeline_gui_handler;
  sigemptyset(&sigact.sa_mask);
  sigact.sa_flags = SA_RESTART;
  if (sigaction(SIGALRM, &sigact, NULL))
    panic("pipeline_gui sigaction failed %s", strerror(errno));
}




static void pipeline_gui_detach_viewfinder()
{
  g_pipeline_gui_display = NULL;
  g_pipeline_gui_window = 0;
}

void pipeline_gui_startup_with_viewfinder(Display *display,
                                          int screen,
                                          Window window,
                                          int viewer_x,
                                          int viewer_y,
                                          int viewer_width,
                                          int viewer_height,
                                          int raw_frame_rate)
{
  fprintf (stderr, "calling pipeline_gui_startup_with_viewfinder\n");
  pipeline_gui_sigaction();      /* sets the callback handler */
  pipeline_gui_attach_viewfinder (display, screen, window, viewer_x,
                                  viewer_y, viewer_width, viewer_height);
  pipeline_gui_interrupt_on(raw_frame_rate); /* start the timer */
}

void pipeline_gui_shutdown()
{ 
  pipeline_gui_interrupt_off();
  pipeline_gui_detach_viewfinder();
}

