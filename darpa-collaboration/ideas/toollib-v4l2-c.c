/* LaHaShem HaAretz U'Mloah */
/* $Id: toollib-v4l2-c.c,v 1.15 2010-10-25 02:58:42 abarbu Exp $ */

/* TODO Andrei Orbit AF supports 10bpp Bayer-packed images */
/* TODO Andrei video capturing support */

/* #defines */
/* V4L2_S2C_BACKTRACES */
/* V4L2_S2C_JPEG_ERROR_FATAL */

#include <execinfo.h>
#include "toollib-v4l2-c.h"
#include "toollib-utils.h"

#define v4l2_unfourcc(fmt)						\
  {V4L2_PIX_FMT_MJPEG & 0xff, V4L2_PIX_FMT_MJPEG >> 8 & 0xff,		\
      V4L2_PIX_FMT_MJPEG >> 16 & 0xff, V4L2_PIX_FMT_MJPEG >> 24 & 0xff, 0}

char* v4l2_video_device[NCAMERAS];
int v4l2_fd[NCAMERAS];
int v4l2_width[NCAMERAS], v4l2_height[NCAMERAS], v4l2_pixformat[NCAMERAS];

unsigned char *v4l2_frame[NCAMERAS], *v4l2_converted_frame[NCAMERAS]
  , *v4l2_temporary_frame[NCAMERAS];
struct camera_buffer *v4l2_buffers[NCAMERAS];
struct v4l2_buffer v4l2_frame_buf[NCAMERAS];

struct jpeg_decdata v4l2_jpeg_decoder;
int v4l2_interrupt_frame_rate;

typedef int (*bgr32fun_t)(const unsigned char *, const int, const int,
			  unsigned char *, unsigned char *);
bgr32fun_t v4l2_bgr_converter[NCAMERAS];

extern char *scrt5_debug_2doutput_2dport_v;

#define CLEAR(x) memset (&(x), 0, sizeof (x))

void print_stacktrace()
{
#ifdef V4L2_S2C_BACKTRACES
  scdebug_dobacktrace(10, 10, 80, scrt5_debug_2doutput_2dport_v);
#endif
}

int v4l2_exit_failure()
{
  void* buf[100];

  backtrace_symbols_fd(buf, backtrace(buf, 100), 0);
#ifdef V4L2_S2C_BACKTRACES
    scdebug_dobacktrace(10, 10, 80, scrt5_debug_2doutput_2dport_v);
#endif
  asm("int3");
  exit(EXIT_FAILURE);
}

/* Maps a v4l2 cid enumeration back to a const char* */
const char* v4l2_cid_to_string(int cid)
{
  switch(cid) {
    //case V4L2_CID_BASE: return "V4L2_CID_BASE"; /* Duplicate value -- AJM */
    //case V4L2_CID_USER_BASE: return "V4L2_CID_USER_BASE"; /* Duplicate value -- AJM */
  case V4L2_CID_BRIGHTNESS: return "V4L2_CID_BRIGHTNESS";
  case V4L2_CID_CONTRAST: return "V4L2_CID_CONTRAST";
  case V4L2_CID_SATURATION: return "V4L2_CID_SATURATION";
  case V4L2_CID_HUE: return "V4L2_CID_HUE";
  case V4L2_CID_AUDIO_VOLUME: return "V4L2_CID_AUDIO_VOLUME";
  case V4L2_CID_AUDIO_BALANCE: return "V4L2_CID_AUDIO_BALANCE";
  case V4L2_CID_AUDIO_BASS: return "V4L2_CID_AUDIO_BASS";
  case V4L2_CID_AUDIO_TREBLE: return "V4L2_CID_AUDIO_TREBLE";
  case V4L2_CID_AUDIO_MUTE: return "V4L2_CID_AUDIO_MUTE";
  case V4L2_CID_AUDIO_LOUDNESS: return "V4L2_CID_AUDIO_LOUDNESS";
  case V4L2_CID_BLACK_LEVEL: return "V4L2_CID_BLACK_LEVEL";
  case V4L2_CID_AUTO_WHITE_BALANCE: return "V4L2_CID_AUTO_WHITE_BALANCE";
  case V4L2_CID_DO_WHITE_BALANCE: return "V4L2_CID_DO_WHITE_BALANCE";
  case V4L2_CID_RED_BALANCE: return "V4L2_CID_RED_BALANCE";
  case V4L2_CID_BLUE_BALANCE: return "V4L2_CID_BLUE_BALANCE";
  case V4L2_CID_GAMMA: return "V4L2_CID_GAMMA";
    //case V4L2_CID_WHITENESS: return "V4L2_CID_WHITENESS"; /* Duplicate value -- AJM */
  case V4L2_CID_EXPOSURE: return "V4L2_CID_EXPOSURE";
  case V4L2_CID_AUTOGAIN: return "V4L2_CID_AUTOGAIN";
  case V4L2_CID_GAIN: return "V4L2_CID_GAIN";
  case V4L2_CID_HFLIP: return "V4L2_CID_HFLIP";
  case V4L2_CID_VFLIP: return "V4L2_CID_VFLIP";
  case V4L2_CID_POWER_LINE_FREQUENCY: return "V4L2_CID_POWER_LINE_FREQUENCY";
  case V4L2_CID_HUE_AUTO: return "V4L2_CID_HUE_AUTO";
  case V4L2_CID_WHITE_BALANCE_TEMPERATURE: return "V4L2_CID_WHITE_BALANCE_TEMPERATURE";
  case V4L2_CID_SHARPNESS: return "V4L2_CID_SHARPNESS";
  case V4L2_CID_BACKLIGHT_COMPENSATION: return "V4L2_CID_BACKLIGHT_COMPENSATION";
  case V4L2_CID_LASTP1: return "V4L2_CID_LASTP1";
  case V4L2_CID_PRIVATE_BASE: return "V4L2_CID_PRIVATE_BASE";
  default: return "unknown cid";
  }
}

/* Maps a v4l2 request enumeration back to a const char* */
const char* v4l2_ioctl_request_code_to_string(long request)
{
  switch(request) {
  case VIDIOC_QUERYCAP: return "VIDIOC_QUERYCAP";
  case VIDIOC_RESERVED: return "VIDIOC_RESERVED";
  case VIDIOC_ENUM_FMT: return "VIDIOC_ENUM_FMT";
  case VIDIOC_G_FMT: return "VIDIOC_G_FMT";
  case VIDIOC_S_FMT: return "VIDIOC_S_FMT";
    //case VIDIOC_G_COMP: return "VIDIOC_G_COMP";
    //case VIDIOC_S_COMP: return "VIDIOC_S_COMP";
  case VIDIOC_REQBUFS: return "VIDIOC_REQBUFS";
  case VIDIOC_QUERYBUF: return "VIDIOC_QUERYBUF";
  case VIDIOC_G_FBUF: return "VIDIOC_G_FBUF";
  case VIDIOC_S_FBUF: return "VIDIOC_S_FBUF";
  case VIDIOC_OVERLAY: return "VIDIOC_OVERLAY";
  case VIDIOC_QBUF: return "VIDIOC_QBUF";
  case VIDIOC_DQBUF: return "VIDIOC_DQBUF";
  case VIDIOC_STREAMON: return "VIDIOC_STREAMON";
  case VIDIOC_STREAMOFF: return "VIDIOC_STREAMOFF";
  case VIDIOC_G_PARM: return "VIDIOC_G_PARM";
  case VIDIOC_S_PARM: return "VIDIOC_S_PARM";
  case VIDIOC_G_STD: return "VIDIOC_G_STD";
  case VIDIOC_S_STD: return "VIDIOC_S_STD";
  case VIDIOC_ENUMSTD: return "VIDIOC_ENUMSTD";
  case VIDIOC_ENUMINPUT: return "VIDIOC_ENUMINPUT";
  case VIDIOC_G_CTRL: return "VIDIOC_G_CTRL";
  case VIDIOC_S_CTRL: return "VIDIOC_S_CTRL";
  case VIDIOC_G_TUNER: return "VIDIOC_G_TUNER";
  case VIDIOC_S_TUNER: return "VIDIOC_S_TUNER";
  case VIDIOC_G_AUDIO: return "VIDIOC_G_AUDIO";
  case VIDIOC_S_AUDIO: return "VIDIOC_S_AUDIO";
  case VIDIOC_QUERYCTRL: return "VIDIOC_QUERYCTRL";
  case VIDIOC_QUERYMENU: return "VIDIOC_QUERYMENU";
  case VIDIOC_G_INPUT: return "VIDIOC_G_INPUT";
  case VIDIOC_S_INPUT: return "VIDIOC_S_INPUT";
  case VIDIOC_G_OUTPUT: return "VIDIOC_G_OUTPUT";
  case VIDIOC_S_OUTPUT: return "VIDIOC_S_OUTPUT";
  case VIDIOC_ENUMOUTPUT: return "VIDIOC_ENUMOUTPUT";
  case VIDIOC_G_AUDOUT: return "VIDIOC_G_AUDOUT";
  case VIDIOC_S_AUDOUT: return "VIDIOC_S_AUDOUT";
  case VIDIOC_G_MODULATOR: return "VIDIOC_G_MODULATOR";
  case VIDIOC_S_MODULATOR: return "VIDIOC_S_MODULATOR";
  case VIDIOC_G_FREQUENCY: return "VIDIOC_G_FREQUENCY";
  case VIDIOC_S_FREQUENCY: return "VIDIOC_S_FREQUENCY";
  case VIDIOC_CROPCAP: return "VIDIOC_CROPCAP";
  case VIDIOC_G_CROP: return "VIDIOC_G_CROP";
  case VIDIOC_S_CROP: return "VIDIOC_S_CROP";
  case VIDIOC_G_JPEGCOMP: return "VIDIOC_G_JPEGCOMP";
  case VIDIOC_S_JPEGCOMP: return "VIDIOC_S_JPEGCOMP";
  case VIDIOC_QUERYSTD: return "VIDIOC_QUERYSTD";
  case VIDIOC_TRY_FMT: return "VIDIOC_TRY_FMT";
  default: return "unknown id";
  }
}

const char* v4l2_ioctl_pixel_format_to_string(int format)
{
  switch(format) {
  case V4L2_PIX_FMT_RGB332: return "V4L2_PIX_FMT_RGB332";
  case V4L2_PIX_FMT_RGB444: return "V4L2_PIX_FMT_RGB444";
  case V4L2_PIX_FMT_RGB555: return "V4L2_PIX_FMT_RGB555";
  case V4L2_PIX_FMT_RGB565: return "V4L2_PIX_FMT_RGB565";
  case V4L2_PIX_FMT_RGB555X: return "V4L2_PIX_FMT_RGB555X";
  case V4L2_PIX_FMT_RGB565X: return "V4L2_PIX_FMT_RGB565X";
  case V4L2_PIX_FMT_BGR24: return "V4L2_PIX_FMT_BGR24";
  case V4L2_PIX_FMT_RGB24: return "V4L2_PIX_FMT_RGB24";
  case V4L2_PIX_FMT_BGR32: return "V4L2_PIX_FMT_BGR32";
  case V4L2_PIX_FMT_RGB32: return "V4L2_PIX_FMT_RGB32";
  case V4L2_PIX_FMT_DV: return "V4L2_PIX_FMT_DV";
  case V4L2_PIX_FMT_ET61X251: return "V4L2_PIX_FMT_ET61X251";
  case V4L2_PIX_FMT_HI240: return "V4L2_PIX_FMT_HI240";
  case V4L2_PIX_FMT_HM12: return "V4L2_PIX_FMT_HM12";
  case V4L2_PIX_FMT_MJPEG: return "V4L2_PIX_FMT_MJPEG";
  case V4L2_PIX_FMT_PWC1: return "V4L2_PIX_FMT_PWC1";
  case V4L2_PIX_FMT_PWC2: return "V4L2_PIX_FMT_PWC2";
  case V4L2_PIX_FMT_SN9C10X: return "V4L2_PIX_FMT_SN9C10X";
  case V4L2_PIX_FMT_WNVA: return "V4L2_PIX_FMT_WNVA";
  case V4L2_PIX_FMT_YYUV: return "V4L2_PIX_FMT_YYUV";
  case V4L2_PIX_FMT_YUV444: return "V4L2_PIX_FMT_YUV444";
  case V4L2_PIX_FMT_YUV555: return "V4L2_PIX_FMT_YUV555";
  case V4L2_PIX_FMT_YUV565: return "V4L2_PIX_FMT_YUV565";
  case V4L2_PIX_FMT_YUV32: return "V4L2_PIX_FMT_YUV32";
  default: return "Unknown pixel format";
  }
}

/** Helper function for v4l2_print_available_controls */
static void print_availabe_controls_enumerate_menu(int camera, const struct v4l2_queryctrl* queryctrl)
{
  struct v4l2_querymenu querymenu;
  printf ("  Menu items:\n");

  memset(&querymenu, 0, sizeof (querymenu));
  querymenu.id = queryctrl->id;

  for (querymenu.index = queryctrl->minimum;
       querymenu.index <= queryctrl->maximum;
       querymenu.index++) {
    if (0 == ioctl (v4l2_fd[camera], VIDIOC_QUERYMENU, &querymenu)) {
      printf ("  %s\n", querymenu.name);
    } else {
      perror ("VIDIOC_QUERYMENU");
      exit (EXIT_FAILURE);
    }
  } 
}

/* Prints the available controls (to stdout) */
void v4l2_print_available_controls(int camera) 
{
  struct v4l2_queryctrl queryctrl;
  
  memset(&queryctrl, 0, sizeof (queryctrl));

  for(int ctrl_id = V4L2_CID_BASE; ctrl_id < V4L2_CID_LASTP1; ++ctrl_id) {
    queryctrl.id = ctrl_id;

    if (0 == ioctl(v4l2_fd[camera], VIDIOC_QUERYCTRL, &queryctrl)) {
      int is_disabled = (queryctrl.flags & V4L2_CTRL_FLAG_DISABLED) ? 1 : 0;
      printf("Control %s%s\n", queryctrl.name, is_disabled ? " -- disabled" : "");

      if (is_disabled) continue;

      if (queryctrl.type == V4L2_CTRL_TYPE_MENU) {
	print_availabe_controls_enumerate_menu(camera, &queryctrl);
      }
    } else {
      if (errno == EINVAL) {
	// printf("0x%08x was skipped\n", ctrl_id);
	continue;
      }

      perror ("VIDIOC_QUERYCTRL");
      exit (EXIT_FAILURE);
    }
  }

  printf("Done!\n");
}

int xioctl (int fd, int request, void *arg)
{
  int r;

  do r = ioctl (fd, request, arg);
  while (-1 == r && EINTR == errno);

  if(r)
  {
    int err = errno;
    printf("v4l2 ioctl %s (%d) failed: %d, errno: %d %s\n", v4l2_ioctl_request_code_to_string(request), request, r, err, strerror(err));
#ifdef V4L2_S2C_BACKTRACES
    scdebug_dobacktrace(10, 10, 80, scrt5_debug_2doutput_2dport_v);
#endif
    printf("\n");
  }

  return r;
}

void errno_exit (const char *s)
{
  fprintf(stderr, "v4l2 %s error %d, %s\n", s, errno, strerror (errno));
  v4l2_exit_failure();
}

int clamp(int v, int l, int u)
{
  return ((v < l) ? l : (v > u ? u : v));
}

int clamp_below(int v, int u) __attribute__((always_inline, pure));

int clamp_below(int v, int u)
{
  return (v > u ? u : v);
}

void yuvToBGRpixel(const unsigned int y,
		   const unsigned char u,
		   const unsigned char v,
		   unsigned char* b,
		   unsigned char* g,
		   unsigned char* r)
  __attribute__((always_inline));

/* YUV -> BGR conversion tables */

int yuvToBGRpixel_table_v_r[] =
  {-179, -178, -177, -175, -174, -172, -171, -170, -168, -167,
   -165, -164, -163, -161, -160, -158, -157, -156, -154, -153,
   -151, -150, -149, -147, -146, -144, -143, -142, -140, -139,
   -137, -136, -135, -133, -132, -130, -129, -128, -126, -125,
   -123, -122, -121, -119, -118, -116, -115, -114, -112, -111,
   -109, -108, -107, -105, -104, -102, -101, -100, -98, -97,
   -95, -94, -93, -91, -90, -88, -87, -86, -84, -83,
   -81, -80, -79, -77, -76, -74, -73, -72, -70, -69,
   -67, -66, -64, -63, -62, -60, -59, -57, -56, -55,
   -53, -52, -50, -49, -48, -46, -45, -43, -42, -41,
   -39, -38, -36, -35, -34, -32, -31, -29, -28, -27,
   -25, -24, -22, -21, -20, -18, -17, -15, -14, -13,
   -11, -10, -8, -7, -6, -4, -3, -1, 0, 1,
   3, 4, 6, 7, 8, 10, 11, 13, 14, 15,
   17, 18, 20, 21, 22, 24, 25, 27, 28, 29,
   31, 32, 34, 35, 36, 38, 39, 41, 42, 43,
   45, 46, 48, 49, 50, 52, 53, 55, 56, 57,
   59, 60, 62, 63, 64, 66, 67, 69, 70, 72,
   73, 74, 76, 77, 79, 80, 81, 83, 84, 86,
   87, 88, 90, 91, 93, 94, 95, 97, 98, 100,
   101, 102, 104, 105, 107, 108, 109, 111, 112, 114,
   115, 116, 118, 119, 121, 122, 123, 125, 126, 128,
   129, 130, 132, 133, 135, 136, 137, 139, 140, 142,
   143, 144, 146, 147, 149, 150, 151, 153, 154, 156,
   157, 158, 160, 161, 163, 164, 165, 167, 168, 170,
   171, 172, 174, 175, 177, 178};

int yuvToBGRpixel_table_v_g[] =
  {-91, -91, -90, -89, -89, -88, -87, -86, -86, -85,
   -84, -84, -83, -82, -81, -81, -80, -79, -79, -78,
   -77, -76, -76, -75, -74, -74, -73, -72, -71, -71,
   -70, -69, -69, -68, -67, -66, -66, -65, -64, -64,
   -63, -62, -61, -61, -60, -59, -59, -58, -57, -56,
   -56, -55, -54, -54, -53, -52, -51, -51, -50, -49,
   -49, -48, -47, -46, -46, -45, -44, -44, -43, -42,
   -41, -41, -40, -39, -39, -38, -37, -36, -36, -35,
   -34, -34, -33, -32, -31, -31, -30, -29, -29, -28,
   -27, -26, -26, -25, -24, -24, -23, -22, -21, -21,
   -20, -19, -19, -18, -17, -16, -16, -15, -14, -14,
   -13, -12, -11, -11, -10, -9, -9, -8, -7, -6,
   -6, -5, -4, -4, -3, -2, -1, -1, 0, 1,
   1, 2, 3, 4, 4, 5, 6, 6, 7, 8,
   9, 9, 10, 11, 11, 12, 13, 14, 14, 15,
   16, 16, 17, 18, 19, 19, 20, 21, 21, 22,
   23, 24, 24, 25, 26, 26, 27, 28, 29, 29,
   30, 31, 31, 32, 33, 34, 34, 35, 36, 36,
   37, 38, 39, 39, 40, 41, 41, 42, 43, 44,
   44, 45, 46, 46, 47, 48, 49, 49, 50, 51,
   51, 52, 53, 54, 54, 55, 56, 56, 57, 58,
   59, 59, 60, 61, 61, 62, 63, 64, 64, 65,
   66, 66, 67, 68, 69, 69, 70, 71, 71, 72,
   73, 74, 74, 75, 76, 76, 77, 78, 79, 79,
   80, 81, 81, 82, 83, 84, 84, 85, 86, 86,
   87, 88, 89, 89, 90, 91};

int yuvToBGRpixel_table_u_g[] =
  {-44, -44, -43, -43, -43, -42, -42, -42, -41, -41,
   -41, -40, -40, -40, -39, -39, -39, -38, -38, -38,
   -37, -37, -36, -36, -36, -35, -35, -35, -34, -34,
   -34, -33, -33, -33, -32, -32, -32, -31, -31, -31,
   -30, -30, -30, -29, -29, -29, -28, -28, -28, -27,
   -27, -26, -26, -26, -25, -25, -25, -24, -24, -24,
   -23, -23, -23, -22, -22, -22, -21, -21, -21, -20,
   -20, -20, -19, -19, -19, -18, -18, -18, -17, -17,
   -17, -16, -16, -15, -15, -15, -14, -14, -14, -13,
   -13, -13, -12, -12, -12, -11, -11, -11, -10, -10,
   -10, -9, -9, -9, -8, -8, -8, -7, -7, -7,
   -6, -6, -6, -5, -5, -4, -4, -4, -3, -3,
   -3, -2, -2, -2, -1, -1, -1, 0, 0, 0,
   1, 1, 1, 2, 2, 2, 3, 3, 3, 4,
   4, 4, 5, 5, 6, 6, 6, 7, 7, 7,
   8, 8, 8, 9, 9, 9, 10, 10, 10, 11,
   11, 11, 12, 12, 12, 13, 13, 13, 14, 14,
   14, 15, 15, 15, 16, 16, 17, 17, 17, 18,
   18, 18, 19, 19, 19, 20, 20, 20, 21, 21,
   21, 22, 22, 22, 23, 23, 23, 24, 24, 24,
   25, 25, 25, 26, 26, 26, 27, 27, 28, 28,
   28, 29, 29, 29, 30, 30, 30, 31, 31, 31,
   32, 32, 32, 33, 33, 33, 34, 34, 34, 35,
   35, 35, 36, 36, 36, 37, 37, 38, 38, 38,
   39, 39, 39, 40, 40, 40, 41, 41, 41, 42,
   42, 42, 43, 43, 43, 44};

int yuvToBGRpixel_table_u_b[] =
  {-227, -225, -223, -221, -220, -218, -216, -214, -213, -211,
   -209, -207, -206, -204, -202, -200, -198, -197, -195, -193,
   -191, -190, -188, -186, -184, -183, -181, -179, -177, -175,
   -174, -172, -170, -168, -167, -165, -163, -161, -159, -158,
   -156, -154, -152, -151, -149, -147, -145, -144, -142, -140,
   -138, -136, -135, -133, -131, -129, -128, -126, -124, -122,
   -120, -119, -117, -115, -113, -112, -110, -108, -106, -105,
   -103, -101, -99, -97, -96, -94, -92, -90, -89, -87,
   -85, -83, -82, -80, -78, -76, -74, -73, -71, -69,
   -67, -66, -64, -62, -60, -58, -57, -55, -53, -51,
   -50, -48, -46, -44, -43, -41, -39, -37, -35, -34,
   -32, -30, -28, -27, -25, -23, -21, -19, -18, -16,
   -14, -12, -11, -9, -7, -5, -4, -2, 0, 2,
   4, 5, 7, 9, 11, 12, 14, 16, 18, 19,
   21, 23, 25, 27, 28, 30, 32, 34, 35, 37,
   39, 41, 43, 44, 46, 48, 50, 51, 53, 55,
   57, 58, 60, 62, 64, 66, 67, 69, 71, 73,
   74, 76, 78, 80, 82, 83, 85, 87, 89, 90,
   92, 94, 96, 97, 99, 101, 103, 105, 106, 108,
   110, 112, 113, 115, 117, 119, 120, 122, 124, 126,
   128, 129, 131, 133, 135, 136, 138, 140, 142, 144,
   145, 147, 149, 151, 152, 154, 156, 158, 159, 161,
   163, 165, 167, 168, 170, 172, 174, 175, 177, 179,
   181, 183, 184, 186, 188, 190, 191, 193, 195, 197,
   198, 200, 202, 204, 206, 207, 209, 211, 213, 214,
   216, 218, 220, 222, 223, 225};

void yuvToBGRpixel(const unsigned int y,
		   const unsigned char u,
		   const unsigned char v,
		   unsigned char* b,
		   unsigned char* g,
		   unsigned char* r)
{
  /* This is the moral equivalent of: */
#if 0
  int r_ = y + 1.402 * (v-128);
  int g_ = y - 0.34414 * (u-128) - 0.71414 * (v-128);
  int b_ = y + 1.772 * (u-128);
#endif

  int r_ = y + yuvToBGRpixel_table_v_r[v];
  int g_ = y - yuvToBGRpixel_table_u_g[u] - yuvToBGRpixel_table_v_g[v];
  int b_ = y + yuvToBGRpixel_table_u_b[u];

  *r = clamp(r_, 0, 255);
  *g = clamp(g_, 0, 255);
  *b = clamp(b_, 0, 255);
}

int yuyv_to_bgr32(const unsigned char *src, const int w, const int h,
		  unsigned char *dst, unsigned char *temp)
{
  int i;
  const int size = w*h;

  for(i = 0; i < size; ++i)
  {
    __builtin_prefetch(src+(i+2)*2);

    yuvToBGRpixel(src[i*2], src[(i/2)*4+1], src[(i/2)*4+3],
  		  &dst[i*4],
  		  &dst[i*4+1],
  		  &dst[i*4+2]);
    dst[i*4+3] = 0;
  }

  return 0;
}

int yuv422_to_bgr32(const unsigned char *src, const int w, const int h,
		    unsigned char *dst, unsigned char *temp)
{
  int i;
  const int size = w*h;

  for(i = 0; i < size; ++i)
  {
    __builtin_prefetch(src+(i+2)*2);

    yuvToBGRpixel(src[i],
		  src[(i/2)+w*h],
		  src[(i/2)+(w*h)+(w*h)/2],
		  &dst[i*4],
		  &dst[i*4+1],
		  &dst[i*4+2]);
    dst[i*4+3] = 0;
  }

  return 0;
}

int yuv420_to_bgr32(const unsigned char *src, const int w, const int h,
		    unsigned char *dst, unsigned char *temp)

{
  int i;
  const int size = w*h;

  for(i = 0; i < size; ++i)
  {
    __builtin_prefetch(src+(i+2)*2);

    yuvToBGRpixel(src[i],
		  src[(w/2)*((i/w)/2) + (i%w)/2 + w*h],
		  src[(w/2)*((i/w)/2) + (i%w)/2 + w*h + w*h/4],
		  &dst[i*4],
		  &dst[i*4+1],
		  &dst[i*4+2]);
    dst[i*4+3] = 0;
  }

  return 0;
}

int mjpeg_to_bgr32(const unsigned char *src, const int w, const int h,
		   unsigned char *dst, unsigned char *temp)
{
  int jh = h, jw = w, ret;

  if((ret = jpeg_decode(&temp, (unsigned char*)src, &jw, &jh, &v4l2_jpeg_decoder)) != 0)
  { fprintf(stderr, "jpeg frame decoding failed %d\n", ret);
#ifdef V4L2_S2C_JPEG_ERROR_FATAL
    v4l2_exit_failure();
#else
    bzero(dst,h*w*4);
    return 1;
#endif
  }

  if(jh != h || jw != w)
  { fprintf(stderr, "jpeg frame header sizes don't match (%dx%d) with originals (%dx%d)\n",
	    jh, jw, h, w);
#ifdef V4L2_S2C_JPEG_ERROR_FATAL
    v4l2_exit_failure();
#else
    bzero(dst,h*w*4);
    return 2;
#endif
  }

  return yuyv_to_bgr32(temp, w, h, dst, NULL);
}

void v4l2_set_video_device(int camera, char *device)
{
  free(v4l2_video_device[camera]);
  v4l2_video_device[camera] = malloc(strlen(device) + 1);
  strcpy(v4l2_video_device[camera], device);
}

int v4l2_available(int camera)
{ struct stat st;

  if (-1 == stat (v4l2_video_device[camera], &st)
      || !S_ISCHR(st.st_mode))
    return false;

  int fd = open (v4l2_video_device[camera], O_RDWR | O_NONBLOCK, 0);

  if(fd < 0)
    return false;

  close(fd);
  return true;
}

void v4l2_open_video(int camera)
{ struct stat st;

  if (-1 == stat (v4l2_video_device[camera], &st)) {
    fprintf(stderr, "v4l2 cannot identify '%s': %d, %s\n",
	    v4l2_video_device[camera], errno, strerror (errno));
    v4l2_exit_failure();
  }

  if (!S_ISCHR (st.st_mode)) {
    fprintf (stderr, "v4l2 %s is not a device\n", v4l2_video_device[camera]);
    v4l2_exit_failure();
  }

  v4l2_fd[camera] = open (v4l2_video_device[camera], O_RDWR | O_NONBLOCK, 0);

  if (v4l2_fd[camera]<0) {
    fprintf (stderr, "v4l2 cannot open '%s': %d, %s\n",
	     v4l2_video_device[camera], errno, strerror (errno));
    v4l2_exit_failure();
  }
}

int v4l2_initialize_video(int camera, int pixformat, int width, int height)
{
  struct v4l2_capability cap;
  struct v4l2_format fmt;

  if (-1 == xioctl(v4l2_fd[camera], VIDIOC_QUERYCAP, &cap)) {
    if (EINVAL == errno) {
      fprintf (stderr, "%s is no V4L2 device\n", v4l2_video_device[camera]);
      v4l2_exit_failure();
    } else
      fprintf(stderr, "VIDIOC_QUERYCAP error %d, %s\n",
	      errno, strerror (errno));
  }

  if (!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)) {
    fprintf(stderr, "%s is not a video capture device\n",
	     v4l2_video_device[camera]);
    v4l2_exit_failure();
  }

  if (!(cap.capabilities & V4L2_CAP_STREAMING)) {
    fprintf(stderr, "%s does not support streaming i/o\n",
	    v4l2_video_device[camera]);
    v4l2_exit_failure();
  }

  switch(pixformat)
  {
  case V4L2_PIX_FMT_YUV420:
    v4l2_bgr_converter[camera] = &yuv420_to_bgr32;
    break;
  case V4L2_PIX_FMT_YUV422P:
    v4l2_bgr_converter[camera] = &yuv422_to_bgr32;
    break;
  case V4L2_PIX_FMT_YUYV:
    v4l2_bgr_converter[camera] = &yuyv_to_bgr32;
    break;
  case V4L2_PIX_FMT_MJPEG:
    v4l2_bgr_converter[camera] = &mjpeg_to_bgr32;
    break;
  default:
    {
      char name[5] = v4l2_unfourcc(V4L2_PIX_FMT_YUYV);
      fprintf(stderr, "v4l2 unsupported pixformat %s(%d), no bgr32 converter\n", name, pixformat);
      v4l2_exit_failure();
    }
  }

  CLEAR(fmt);

  fmt.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  fmt.fmt.pix.width = width;
  fmt.fmt.pix.height = height;
  fmt.fmt.pix.pixelformat = pixformat;
  fmt.fmt.pix.field = V4L2_FIELD_NONE;
  fmt.fmt.pix.colorspace = V4L2_COLORSPACE_SRGB;

  if (-1 == xioctl(v4l2_fd[camera], VIDIOC_S_FMT, &fmt))
  {
    fprintf(stderr, "Failed to set camera pixel format (to %s)...\n", v4l2_ioctl_pixel_format_to_string(pixformat));
    v4l2_uninitialize_video(camera);
    return 1;
  }

  v4l2_width[camera] = width;
  v4l2_height[camera] = height;
  v4l2_pixformat[camera] = pixformat;
  v4l2_frame[camera] = malloc(width*height*4);
  v4l2_converted_frame[camera] = malloc(width*height*4);
  v4l2_temporary_frame[camera] = malloc(width*height*4);

  v4l2_initialize_mmap(camera);
  return 0;
}

void v4l2_initialize_mmap(int camera)
{
  struct v4l2_requestbuffers req;
  int n_buffers;

  CLEAR (req);

  req.count = 4;
  req.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  req.memory = V4L2_MEMORY_MMAP;

  if (-1 == xioctl (v4l2_fd[camera], VIDIOC_REQBUFS, &req)) {
    if (EINVAL == errno) {
      fprintf (stderr, "v4l2 %s does not support mmaped i/o\n",
	       v4l2_video_device[camera]);
      v4l2_exit_failure();
    } else {
      errno_exit ("VIDIOC_REQBUFS");
    }
  }

  if (req.count < 2) {
    fprintf (stderr, "v4l2 Insufficient memory for buffers on %s\n",
	     v4l2_video_device[camera]);
    v4l2_exit_failure();
  }

  v4l2_buffers[camera] = calloc(req.count + 1, sizeof (struct v4l2_buffer));

  if (!v4l2_buffers[camera]) {
    perror("Out of memory\n");
    v4l2_exit_failure();
  }

  for (n_buffers = 0; n_buffers < req.count; ++n_buffers) {
    struct v4l2_buffer buf;

    CLEAR (buf);

    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;
    buf.index = n_buffers;

    if (-1 == xioctl (v4l2_fd[camera], VIDIOC_QUERYBUF, &buf))
      errno_exit ("VIDIOC_QUERYBUF");

    v4l2_buffers[camera][n_buffers].length = buf.length;
    v4l2_buffers[camera][n_buffers].start =
      mmap (NULL /* start anywhere */,
	    buf.length,
	    PROT_READ | PROT_WRITE /* required */,
	    MAP_SHARED /* recommended */,
	    v4l2_fd[camera], buf.m.offset);

    if (MAP_FAILED == v4l2_buffers[camera][n_buffers].start)
      errno_exit ("mmap");
  }

  v4l2_buffers[camera][n_buffers].start = 0;
  v4l2_buffers[camera][n_buffers].length = -1;
}


void v4l2_uninitialize_mmap(int camera)
{
  v4l2_stop_capturing(camera);

  if(v4l2_buffers[camera])
  {
    unsigned int i = 0;

    while(v4l2_buffers[camera][i].length != -1)
    {
      if (-1 == munmap (v4l2_buffers[camera][i].start,
			v4l2_buffers[camera][i].length))
	errno_exit ("munmap");
      v4l2_buffers[camera][i].length= -1;
      v4l2_buffers[camera][i].start = 0;
      i++;
    }

    free(v4l2_buffers[camera]);
    v4l2_buffers[camera] = NULL;
  }
}

void v4l2_uninitialize_video(int camera)
{
  v4l2_uninitialize_mmap(camera);
  v4l2_bgr_converter[camera] = NULL;
  v4l2_width[camera] = -1;
  v4l2_height[camera] = -1;
  v4l2_pixformat[camera] = -1;
  free(v4l2_frame[camera]);
  v4l2_frame[camera] = NULL;
  free(v4l2_converted_frame[camera]);
  v4l2_converted_frame[camera] = NULL;
  free(v4l2_temporary_frame[camera]);
  v4l2_temporary_frame[camera] = NULL;
}


void v4l2_stop_capturing (int camera)
{
  enum v4l2_buf_type type;

  type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if (-1 == xioctl (v4l2_fd[camera], VIDIOC_STREAMOFF, &type))
    errno_exit ("VIDIOC_STREAMOFF");
}

void v4l2_start_capturing(int camera)
{
  unsigned int i = 0;
  enum v4l2_buf_type type;

  while(v4l2_buffers[camera][i].length != -1)
  {
    struct v4l2_buffer buf;

    CLEAR (buf);

    buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    buf.memory = V4L2_MEMORY_MMAP;
    buf.index = i;

    if (-1 == xioctl (v4l2_fd[camera], VIDIOC_QBUF, &buf))
      errno_exit ("VIDIOC_QBUF");

    i++;
  }

  type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

  if (-1 == xioctl (v4l2_fd[camera], VIDIOC_STREAMON, &type))
    errno_exit ("VIDIOC_STREAMON");
}


void v4l2_close_video(int camera)
{
  v4l2_uninitialize_video(camera);

  int ret = close(v4l2_fd[camera]);
  if (ret == EINTR)
    v4l2_close_video(camera);
  if (ret !=0 && ret != EBADF)
  { perror("v4l2 close failed");
    exit(-1);}
}

int v4l2_set_control(int camera, int id, int value)
{
  struct v4l2_control control;
  bzero(&control, sizeof(struct v4l2_control));
  control.id = id;
  control.value = value;
  return 0 == xioctl(v4l2_fd[camera], VIDIOC_S_CTRL, &control);
}

int v4l2_get_control(int camera, int id)
{
  struct v4l2_control control;
  bzero(&control, sizeof(struct v4l2_control));
  control.id = id;
  control.value = 0;
  return 0 == xioctl(v4l2_fd[camera], VIDIOC_G_CTRL, &control);
}

void v4l2_initialize()
{
  int i;
  for(i = 0; i < NCAMERAS; ++i)
  {
    v4l2_video_device[i] = malloc(101);
    snprintf(v4l2_video_device[i], 100, "/dev/video%d", i);

    v4l2_fd[i] = -1;
    v4l2_width[i] = -1;
    v4l2_height[i] = -1;
    v4l2_pixformat[i] = -1;
    v4l2_frame[i] = NULL;
    v4l2_converted_frame[i] = NULL;
    v4l2_temporary_frame[i] = NULL;
    v4l2_buffers[i] = NULL;
    v4l2_bgr_converter[i] = NULL;
  }
}

void v4l2_deinitialize()
{
  printf("Hello world!\n");
}

int v4l2_frame_available(int camera)
{ if(v4l2_fd[camera] < 0)
    return 0;

  fd_set readfds;
  struct timeval timeval;
  int retval;
  FD_ZERO(&readfds);
  FD_SET(v4l2_fd[camera], &readfds);
  timeval.tv_sec = 0;
  timeval.tv_usec = 0;
  retval = select(v4l2_fd[camera]+1, &readfds, NULL, NULL, &timeval);
  if (retval==-1)
  {
    if (EINTR == errno)
      v4l2_frame_available(camera);
    else
    {
      perror("v4l2 select failed");
      exit(-1);}}

  return retval > 0;
}

int v4l2_discard_frame(int camera)
{
  struct v4l2_buffer buf;
  CLEAR (buf);
  buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buf.memory = V4L2_MEMORY_MMAP;

  if (0 < xioctl (v4l2_fd[camera], VIDIOC_DQBUF, &buf)) {
    switch (errno) {
    case EAGAIN: return 1;
    default: errno_exit ("VIDIOC_DQBUF");
    }
  }

  if (-1 == xioctl (v4l2_fd[camera], VIDIOC_QBUF, &buf))
    errno_exit ("VIDIOC_QBUF");

  return 0;
}

int v4l2_get_frame(int camera)
{
  struct v4l2_buffer buf;

  CLEAR (buf);

  buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
  buf.memory = V4L2_MEMORY_MMAP;

  if (0 < xioctl (v4l2_fd[camera], VIDIOC_DQBUF, &buf)) {
    switch (errno) {
    case EAGAIN:
      return 1;

    case EIO:
      /* Not necessarely fatal, could do something more intelligent
	 like retry */

    default:
      errno_exit ("VIDIOC_DQBUF");
    }
  }

  if(v4l2_buffers[camera][buf.index].length >
     v4l2_width[camera]*v4l2_height[camera]*4)
  {
    fprintf(stderr, "v4l2 camera ring buffer is larger than the frame buffer (%d > %d) \n",
	    (int)v4l2_buffers[camera][buf.index].length,
	    v4l2_width[camera]*v4l2_height[camera]*4);
    v4l2_exit_failure();
  }

  memcpy(v4l2_frame[camera], v4l2_buffers[camera][buf.index].start,
	 buf.bytesused);
  v4l2_frame_buf[camera] = buf;

  if (-1 == xioctl (v4l2_fd[camera], VIDIOC_QBUF, &buf))
    errno_exit ("VIDIOC_QBUF");

  return
    v4l2_bgr_converter[camera](v4l2_frame[camera], v4l2_width[camera],
			       v4l2_height[camera],
			       v4l2_converted_frame[camera],
			       v4l2_temporary_frame[camera]);
}

void v4l2_interrupt_on(int frame_rate)
{ if(v4l2_interrupt_frame_rate &&
     v4l2_interrupt_frame_rate < frame_rate)
    return;

  v4l2_interrupt_frame_rate = frame_rate;
  struct itimerval itimerval;
  itimerval.it_interval.tv_sec = 0;
  itimerval.it_interval.tv_usec = 1000000/frame_rate;
  itimerval.it_value.tv_sec = 0;
  itimerval.it_value.tv_usec = 1000000/frame_rate;
  setitimer(ITIMER_REAL, &itimerval, (struct itimerval *)0);}

void v4l2_interrupt_off(void)
{ v4l2_interrupt_frame_rate = 0;
  struct itimerval itimerval;
  itimerval.it_interval.tv_sec = 0;
  itimerval.it_interval.tv_usec = 0;
  itimerval.it_value.tv_sec = 0;
  itimerval.it_value.tv_usec = 0;
  setitimer(ITIMER_REAL, &itimerval, (struct itimerval *)0);}

void v4l2_write_ppm(int camera, char *name)
{ FILE *f;
  int y, x;
  f = fopen(name, "w");
  fprintf(f, "P6\n");
  fprintf(f, "%d %d\n", v4l2_width[camera], v4l2_height[camera]);
  fprintf(f, "255\n");
  for (y = 0; y<v4l2_height[camera]; y++)
  { for (x = 0; x<v4l2_width[camera]; x++)
    { putc(v4l2_converted_frame[camera][(y*v4l2_width[camera]+x)*4+2], f);
      putc(v4l2_converted_frame[camera][(y*v4l2_width[camera]+x)*4+1], f);
      putc(v4l2_converted_frame[camera][(y*v4l2_width[camera]+x)*4+0], f);}}
  fclose(f);}

int v4l2_wait_for_frame(int camera)
{
  int timeouts = 0;
  while(1)
  {
    fd_set fds;
    struct timeval tv;

    FD_ZERO (&fds);
    FD_SET (v4l2_fd[camera], &fds);

    /* Timeout. */
    tv.tv_sec = 2;
    tv.tv_usec = 0;

    int r = select (v4l2_fd[camera] + 1, &fds, NULL, NULL, &tv);

    if (-1 == r) {
      if (EINTR == errno)
	continue;

      errno_exit ("select");
    }

    if (0 == r) {
      timeouts++;
      if(timeouts < 5) { printf("v4l2 frame delay\n"); continue; }
      fprintf (stderr, "select timeout\n");
      exit (EXIT_FAILURE);
    }

    break;
  }

  return 1;
}

/* Tam V'Nishlam Shevah L'El Borei Olam */
