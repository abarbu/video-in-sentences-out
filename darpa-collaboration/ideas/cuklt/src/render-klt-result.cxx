/**
 * @file make-klt-video.cxx
 *
 *  @remark ~ This program create a video showing the klt result.
 *
 *  @author ~ Tommy Chang
 *
 *  @par Created by Tommy Chang, 2011-12-30 (Fri) 
 **/


/*----------------------------------*
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <getopt.h>
#include <string.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>
#include <unistd.h>

#include <sys/stat.h>
#include <opencv/highgui.h>
#include <cv.h>
#include <errno.h>
#include "zip.h"
#include "ff-light.h"

typedef struct {
  int         debugLevel_bitored;
  const char *statusFile_str;
  const char *videoFile_str;
  const char *kltFile_str;
  int         display_int;
} CommandLineParamType;

typedef struct  {
  float x;
  float y;
}  KLT_FeatureRec;


/*---------------------------------------*
 * Global Static Variables and Constants *
 \*-------------------------------------*/
static int fprintf_count = 0;
static CommandLineParamType g_clParams = {
  0,                            // debugLevel_bitored
  NULL,                         // statusFile_str
  NULL,                         // videoFile_str
  NULL,                         // kltFile_str
  0,                            // display_int
};

static IplImage*  g_cimg = NULL; // current color image
static int  g_quit_int = 0;
static bool g_executing_flag = false;
#define cvWindowName "KLT Result"



/*------------------*
 * Macro functions  *
 \*----------------*/
#if defined (__MINGW32__) || defined (__GNUC__) 
#  define fprintf(df, ...)                                      \
     do {                                                       \
       if (df == stderr)                                        \
         {                                                      \
           fprintf (stderr, "[make-klt-video %d]: ",            \
                    fprintf_count++);                           \
           fprintf (stderr, __VA_ARGS__);                       \
           fflush (stderr);                                     \
         }                                                      \
       else                                                     \
         fprintf (df, __VA_ARGS__);                             \
     } while (0)
#endif

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
#ifdef NDEBUG
#define fakeUseParam(...)
#else
static void fakeUseParam(...) {}
#endif



/*-------------------------*
 * Private Implementations *
 \*-----------------------*/
static void 
CleanUps ()
{
  fprintf (stderr, "Doing CleanUps\n");
  fprintf (stderr, "Done CleanUps\n");
  if (g_clParams.display_int)
    {
      cvDestroyWindow (cvWindowName);
    }
}


static void
ExitFailure (const char *mesg_str = NULL)
{
  if (mesg_str)
    fprintf (stderr, "%s\n", mesg_str);

  if (g_clParams.statusFile_str)
    {
      FILE *sFileP = fopen (g_clParams.statusFile_str, "wb");
      fprintf (sFileP, "0"); // write 0 to status file -- failure
      fclose (sFileP);
    }
  exit (EXIT_FAILURE);
}


static void 
HandleSignal (int)
{
  if (g_executing_flag)
    g_quit_int = 1;
  else
    {
      if (g_clParams.statusFile_str)
        {
          FILE *sFileP = fopen (g_clParams.statusFile_str, "wb");
          fprintf (sFileP, "0"); // write 0 to status file -- failure
          fclose (sFileP);
        }
      exit (EXIT_SUCCESS);
    }
}


static double 
mstime (void)
{
  double m;
  struct timeval tp;
  if (gettimeofday(&tp, 0) != 0)
    return 0;
  m = ((double) tp.tv_usec / 1000.0) + (tp.tv_sec * 1000);
  return m;
}


static void
CheckRequiredOptions ()
{


}


static void
SetSystemReady ()
{
  // system ready:
  if (g_clParams.statusFile_str)
    {
      FILE *sFileP = fopen (g_clParams.statusFile_str, "wb");
      fprintf (sFileP, "%d", getpid ()); // success = pid to status file 
      fclose (sFileP);
    }
}


static bool
GetImageSizeFromVideoFile (const char *fileName_str,
                           int        *imgWidth_intP,
                           int        *imgHeight_intP)
{
  fprintf (stderr, "Calling '%s'\n", __FUNCTION__);
  CvCapture* cvCaptureP = cvCreateFileCapture (fileName_str);
  if (cvCaptureP == NULL)
    {
      fprintf (stderr, "[Error]: could not open video file '%s'\n",
               g_clParams.videoFile_str);
      return false;
    }
  IplImage* img = cvQueryFrame (cvCaptureP); // TC 2011-11-23 (Wed)
                                             // --don't release this
                                // img, otherwise cvReleaseCapture will
                                // seg fault
  if (img == NULL)
    {
      fprintf (stderr, "[Error]: could not open video file '%s'\n",
               g_clParams.videoFile_str);
      return false;
    }

  if (imgWidth_intP)
    *imgWidth_intP = img->width;
  if (imgHeight_intP)
    *imgHeight_intP = img->height;

  cvReleaseCapture (&cvCaptureP);
  return true;
}


static bool
InitSystem ()
{
  /*
   * Get image dimension:
   */
  int imgWidth_int, imgHeight_int;
  if (GetImageSizeFromVideoFile (g_clParams.videoFile_str, 
                                 &imgWidth_int, &imgHeight_int) == false)
    {
      fprintf (stderr, "[Error]: could not open vidoe file '%s'\n",
               g_clParams.videoFile_str);
      return false;
    }


  CvSize img_size = cvSize (imgWidth_int, imgHeight_int);
  g_cimg = cvCreateImage (img_size, IPL_DEPTH_8U, 4); 


  /*
   * Create OpenCV Display:
   */
  if (g_clParams.display_int)
    {
      cvNamedWindow (cvWindowName, CV_WINDOW_AUTOSIZE);	
    }
  return true;
}


static struct zip*
OpenKltZip (const char* zipFile_str)
{
  int err;
  struct zip *zs = zip_open (zipFile_str, 0, &err);
  char errstr[1024];
  if (zs == NULL) 
    {
      zip_error_to_str (errstr, sizeof(errstr), err, errno);
      fprintf(stderr, "cannot open zip archive `%s': %s\n",
              g_clParams.kltFile_str, errstr);
      exit (EXIT_FAILURE);
    }
  return zs;
}

static void
exp ()
{
  int err;
  struct zip *zs = zip_open (g_clParams.kltFile_str, 0, &err);
  char errstr[1024];
  if (zs == NULL) 
    {
      zip_error_to_str (errstr, sizeof(errstr), err, errno);
      fprintf(stderr, "cannot open zip archive `%s': %s\n",
              g_clParams.kltFile_str, errstr);
      exit (EXIT_FAILURE);
    }
  
  // if ((idx=zip_name_locate(za, fname, name_flags)) != -1) {
  const char *fname;
  for (int idx = 0; idx < zip_get_num_files (zs); idx++) 
    {
      fname = zip_get_name (zs, idx, 0);
      struct zip_stat st;
      zip_stat_index (zs, idx, 0, &st);
      fprintf (stderr, "fname is %s, size=%d\n", fname, (int) st.size);

      struct zip_file *zf;
      zf = zip_fopen_index (zs, idx, 0);
      if (zf == NULL) 
        {
          fprintf(stderr, "cannot open file %d in archive: %s\n",
                  idx, zip_strerror (zs));
          zip_fclose(zf);
          exit (EXIT_FAILURE);
        }

      char buffer [st.size+1];

      zip_fread (zf, buffer, st.size);
      buffer [st.size] = '\0';
      fprintf (stderr, "%s\n", buffer);
      fprintf (stderr, "--------------\n");
      zip_fclose(zf);
    }
}

static void
ReadKltPairs (int             pair_num,
              struct zip*     za,
              KLT_FeatureRec* curr_P,
              KLT_FeatureRec* next_P,
              int*            nFeatures_P)
{
  fakeUseParam (pair_num, za, curr_P, next_P, nFeatures_P);

  /*
   * The name of the klt pair file:
   */
  char dirName[16];
  sprintf (dirName, "%06d", pair_num);
  zip_add_dir (za, dirName);
  char fileName[16];
  sprintf (fileName, "%s/klt.text", dirName);

  /*
   * Determine the klt pair file location in the zip file:
   */
  int idx = zip_name_locate (za, fileName, 0); 
  if (idx == -1)
    {
      fprintf (stderr, "could not locate file %s in zip \n", fileName);
      exit (EXIT_FAILURE);
    }
  
  struct zip_stat st;
  zip_stat_index (za, idx, 0, &st);
  // fprintf (stderr, "fname is %s, size=%d\n", fileName, (int) st.size);

  /*
   * Read in the klt pair:
   */
  struct zip_file *zf;
  zf = zip_fopen_index (za, idx, 0);
  if (zf == NULL) 
    {
      fprintf(stderr, "cannot open file %d in archive: %s\n",
              idx, zip_strerror (za));
      zip_fclose(zf);
      exit (EXIT_FAILURE);
    }

  char buffer [st.size+1];
  zip_fread (zf, buffer, st.size);
  buffer [st.size] = '\0';
  zip_fclose (zf);

  /*
   * Copy klt pair data:
   */
  int f_idx = 0;
  char *lineP = &(buffer[0]);
  while (true)
    {
      int nRead = sscanf (lineP, "%f %f %f %f",
                          &(curr_P[f_idx].x), &(curr_P[f_idx].y),
                          &(next_P[f_idx].x), &(next_P[f_idx].y));
      lineP = strstr (lineP, "\n");
      if (lineP == NULL)
        break;
      assert (nRead == 4);
      lineP ++;
      f_idx ++;
    }
  assert (f_idx < 1000);
  *nFeatures_P = f_idx;
}


static IplImage*
LoadVideoFrameToGrey (video_handle videoP)
{
  if (ff_video_finished (videoP))
    return NULL;

  Imlib_Image* handle = ff_get_frame_as_imlib (videoP);
  if (handle == NULL)
    return NULL;

  imlib_context_set_image (handle);
  DATA32* raw = imlib_image_get_data_for_reading_only(); // ARGB handle
  assert (g_cimg->widthStep == (int) (g_cimg->width * sizeof (DATA32)));
  memcpy (g_cimg->imageData, raw, 
          g_cimg->width * g_cimg->height * sizeof (DATA32));
  
  /*
   * convert to 8-bit grey:
   */
  CvSize img_size = cvSize (g_cimg->width, g_cimg->height);
  IplImage* gimg = cvCreateImage (img_size, IPL_DEPTH_8U, 1);
  cvCvtColor (g_cimg, gimg, CV_RGBA2GRAY); // RGBA is misleading, the
                                           // first byte is B followed
                                           // by G, R, and A. in Little Endian.
                                           // But uint32 is always 0xAARRGGBB

  /*
   * create a 32-bit grey:
   */
  IplImage* gimgf =  cvCreateImage (img_size, IPL_DEPTH_32F, 1); 
  cvConvertScale (gimg, gimgf, 1.0, 0.0);

  /*
   * clean ups:
   */
  cvReleaseImage (&gimg);
  imlib_free_image ();
  ff_next_frame (videoP);

  return gimgf;
}

static const char*
basepart (const char *file_name)
{
  static char base_part[1024];
  snprintf (base_part, sizeof (base_part), "%s", file_name);
  assert (strlen (base_part) < 1024);

  char *dotPtr = strrchr (base_part, '.');
  if (dotPtr != NULL)
    *dotPtr = '\0';

  char *slashPtr = strrchr (base_part, '/');
  if (slashPtr != NULL)
    return slashPtr + 1;

  slashPtr = strrchr (base_part, '\\');
  if (slashPtr != NULL)
    return slashPtr + 1;

  return base_part;
}


static void
SaveRendering (IplImage* imgP,
               int       frameIdx)
{
  char dirName[16];
  sprintf (dirName, "%06d", frameIdx);
  mkdir (dirName, 0777);

  char fileName[256];
  snprintf (fileName, sizeof (fileName), "%s/%s.jpg", dirName, 
            basepart (g_clParams.kltFile_str));

  fprintf (stderr, "saving to file %s\n", fileName);
  cvSaveImage (fileName, imgP);
}


static void
Run ()
{
  fprintf (stderr, "Calling %s\n", __FUNCTION__);
  /*--------------------------------------
   * Check Require Commandline options:
   *------------------------------------*/
  CheckRequiredOptions ();

  /*-----------------------------
   * subsystems initialization:
   *---------------------------*/
  if (InitSystem ())
    SetSystemReady ();
  else
    ExitFailure ();

  // exp ();
  const int nFeatures = 1000;
  video_handle videoP = ff_open_video (g_clParams.videoFile_str);
  assert (videoP);

  struct zip *zs = OpenKltZip (g_clParams.kltFile_str);
  assert (zs);

  /*-----------------------
   * controller loop:
   *---------------------*/
  g_executing_flag = true;
  LoadVideoFrameToGrey (videoP); // skip the first frame
  int frameIdx = 1;              //  ..
  int nTrackedFeatures = 0;
  KLT_FeatureRec tc_currFeaRecs[nFeatures];
  KLT_FeatureRec tc_nextFeaRecs[nFeatures];
  while (g_quit_int == 0)
    {
      /*
       * Get current image frame:
       */
      IplImage* gimgf = LoadVideoFrameToGrey (videoP);
      if (unlikely (gimgf == NULL))
        break;

      /*
       * Read-in the klt pair:
       */
      ReadKltPairs (frameIdx, zs, tc_currFeaRecs, tc_nextFeaRecs, 
                    &nTrackedFeatures);

      /*
       * Overlay the klt pair onto image:
       */
      CvPoint p0, p1;
      int scale = 1.0;
      for (int f_idx = 0; f_idx < nTrackedFeatures; f_idx++)
        {
          p0.x = (int)(scale*(tc_currFeaRecs[f_idx].x) + 0.5f);
          p0.y = (int)(scale*(tc_currFeaRecs[f_idx].y) + 0.5f);
          p1.x = (int)(scale*(tc_nextFeaRecs[f_idx].x) + 0.5f);
          p1.y = (int)(scale*(tc_nextFeaRecs[f_idx].y) + 0.5f);

          // fprintf (stderr, "(%d %d) to (%d %d)\n",
          //          p0.x, p0.y, p1.x, p1.y);
          cvLine (g_cimg, p1, p0, CV_RGB(0, 255, 0), 2);  // Green
          cvCircle (g_cimg, p1, 1, CV_RGB(255, 0, 0), 4); // Red
        }

      /*
       * Display the result:
       */
      if (g_clParams.display_int) 
        {
          cvShowImage (cvWindowName, g_cimg);
          char ch = (char) cvWaitKey (g_clParams.display_int);
          if (ch == 'q') 
            break;
        }

      /*
       * Save the rendering frames:  (use feh to navigate through them)
       */
      SaveRendering (g_cimg, frameIdx);
      

      /*
       * Next frame
       */
      cvReleaseImage (&gimgf);
      frameIdx++;
    }

  //CleanUps_lbl:
  ff_close_and_free_video (videoP);
  CleanUps ();
}



static void
PrintUsage (char *progname_param)
{
  char *progname = strrchr (progname_param, '\\');
  if (progname == NULL)
    progname = progname_param;
  else
    progname += 1;

  fprintf 
    (stderr,
     "\n\nLast compiles on: %s\n"
     "Usage: %s [options] klt_file video_file:\n"
     "[options] =\n"
     "  --help                Show this usage instruction.\n"
     "  --debug <int>         <int> : level of verboseness;\n" 
     "                            can be multi-valued; default 0.\n" 
     "  --statusFile <str>    <str> : output status file.\n" 
     "                            The status file contains the pid \n" 
     "                            if success, otherwise 0.\n" 
     " \n"
     "  --display <int>       Display the KLT; <int> = delay [ms] (e.g. 5)\n"
     "                        <int> = 0 will disable display (default)\n"
     "\n"
     "examples:\n"
     "  %s video.mov klt.zip\n"
     "  %s video.avi klt.zip\n"
     "\n", __DATE__
     , progname
     , progname
     , progname);
  exit (0);
}



static void
ParseCmdLine (int    argc,
              char **argv)
{
  /*---------------------------------
   * be intelligent about arguments
   *-------------------------------*/
  static struct option longOpt[] = {
    /*name, has_arg, int* flg, if flg: *flg = retval, ow. get_long() = retval*/
    {"debug",           1, NULL,        0},
    {"help",            0, NULL,       'h'},
    {"statusFile",      1, NULL,        0},
    {0,0,0,0} // terminator
  };


  if (argc == 1)
    PrintUsage (argv[0]);

  int opt = 0;
  int optIdx = 0;
# define IsOpt_M(pname) else if (strcmp (longOpt [optIdx].name, pname) == 0)
  while ((opt = getopt_long_only (argc, argv, "", longOpt, &optIdx)) != -1)
    {
      switch (opt)              // optarg is the argument
        {
        case 0:
          if (longOpt [optIdx].flag) // auto assigned value
            break;
          IsOpt_M ("debug")
            g_clParams.debugLevel_bitored |= (1 << (atoi (optarg) - 1));
          IsOpt_M ("statusFile")
            g_clParams.statusFile_str = optarg;
          else 
            fprintf (stderr, "un-handled option %s\n", longOpt [optIdx].name);
          break;
        case 'h':               // get help
          PrintUsage (argv[0]);
          exit (EXIT_SUCCESS);
          break;
        case '?':               // unrecognized option, no more msg needed
          ExitFailure ();
          break;
        default:
          printf ("%c option not implemented yet\n", opt);
          break;
        }
    }
# undef IsOpt_M

  /* Here are the rest of parameters: */
  /* we are expecting a video and its klt output */
  if ((argc - optind) != 2)
    {
      fprintf (stderr, "expecting a video and its klt output\n");
      ExitFailure ();      
    }
  g_clParams.videoFile_str = argv[optind];
  g_clParams.kltFile_str = argv[optind+1];
}



/*------------------------*
 * Public Implementations *
 \*----------------------*/
int
main (int    argc,
      char **argv)
{
  /*------------------------------
   * parse command-line arguments:
   *----------------------------*/
  ParseCmdLine (argc, argv);


  /*-----------------------------
   * interrupt signals setup:
   *---------------------------*/
  signal (SIGINT, HandleSignal);
  signal (SIGHUP, HandleSignal);


  /*-----------------------------
   * run system:
   *---------------------------*/
  Run ();


  /*-----------------------------
   * program termination:
   *---------------------------*/
  fprintf (stderr, "Program Terminated Properly\n");
  return (EXIT_SUCCESS);
}
