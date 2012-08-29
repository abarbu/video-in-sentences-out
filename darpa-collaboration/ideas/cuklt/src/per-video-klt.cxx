/**
 * @file per-video-klt.cxx
 *
 *  @remark ~ Per-Video KLT using CUDA library.
 *
 *  @author ~ Tommy Chang
 *
 *  @par Created by Tommy Chang, 2011-11-23 (Wed) 
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
#include "ff-light.h"
#include "featurePool.h"

typedef struct {
  const char *videoFile_str;
  int         numFeatures_int;
  int         pyramidLevels_int;
  int         maskSize_int;
  int         templateSize_int;
  int         smoothing_int;
  int         display_int;
  int         saveFrames_int;
} CommandLineParamType;


/*---------------------------------------*
 * Global Static Variables and Constants *
 \*-------------------------------------*/
static IplImage*  g_cimg = NULL; // current color image
static IplImage*  g_gimg = NULL; // current grey image
static CFeature2DPool* featurePool = NULL;
static int fprintf_count = 0;
static CommandLineParamType g_clParams = {
  NULL,                         // videoFile_str
  150,                          // numFeatures_int
  3,                            // pyramidLevels_int
  15,                           // maskSize_int
  11,                           // templateSize_int
  3,                            // smoothing_int
  0,                            // display_int
  0,                            // saveFrames_int
};
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
           fprintf (stderr, "[per-video-klt %d]: ",             \
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
  // fprintf (stderr, "Doing CleanUps\n");
  // fprintf (stderr, "Done CleanUps\n");

  if (g_clParams.display_int)
    {
      cvDestroyWindow (cvWindowName);
    }

  if (featurePool) 
    delete featurePool;
}


static void
ExitFailure (const char *mesg_str = NULL)
{
  if (mesg_str)
    fprintf (stderr, "%s\n", mesg_str);
  exit (EXIT_FAILURE);
}


static void 
HandleSignal (int)
{
  if (g_executing_flag)
    g_quit_int = 1;
  else
    exit (EXIT_SUCCESS);
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

  // test image read ok or not..
  // fprintf (stderr, "img dim is (%d %d)\n", img->width, img->height);
  // cvSaveImage("test.jpg", img);

  if (imgWidth_intP)
    *imgWidth_intP = img->width;
  if (imgHeight_intP)
    *imgHeight_intP = img->height;


  // /*
  //  * Test image size limit:
  //  */
  // // if (imgWidth_intP) *imgWidth_intP = 640;       // columns
  // // if (imgHeight_intP) *imgHeight_intP = 480;      // rows
  // if (imgWidth_intP) *imgWidth_intP = 1280;       // columns
  // if (imgHeight_intP) *imgHeight_intP = 600;      // rows
  

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

  /*
   * Create feature pool:
   */
  CvSize img_size = cvSize (imgWidth_int, imgHeight_int);
  featurePool = new CFeature2DPool (img_size, 
                                    g_clParams.templateSize_int,
                                    g_clParams.pyramidLevels_int, 
                                    g_clParams.maskSize_int, 
                                    true, NULL);

  g_cimg = cvCreateImage (img_size, IPL_DEPTH_8U, 4); 
  g_gimg = cvCreateImage (img_size, IPL_DEPTH_8U, 3); 

  /*
   * Create OpenCV Display:
   */
  if (g_clParams.display_int)
    {
      cvNamedWindow (cvWindowName, CV_WINDOW_AUTOSIZE);	
    }

  return true;
}


static void 
smooth_image_float (const IplImage* gimg, // original 8-bit gray image
                    IplImage*       gimgf, // output 32-bit grey image
                    int             w)
{
  CvSize size = cvSize (gimg->width, gimg->height);
  IplImage* temp = cvCreateImage (size, IPL_DEPTH_32F, 1);
  cvConvertScale (gimg, temp, 1.0, 0.0);
  
  // Smoothing
  int s = ((w % 2) == 0) ? w + 1 : w; // make it odd
  cvSmooth (temp, gimgf, CV_GAUSSIAN, s, s);
  cvReleaseImage (&temp);
}


static IplImage*
LoadVideoFrameTo32BitGrey (video_handle videoP)
{
  if (ff_video_finished (videoP))
    return NULL;

  Imlib_Image* handle = ff_get_frame_as_imlib (videoP);
  if (handle == NULL)
    return NULL;

  imlib_context_set_image (handle);
  DATA32* raw = imlib_image_get_data_for_reading_only(); // ARGB handle
  // fprintf (stderr, "%d \n", g_cimg->width);
  // fprintf (stderr, "%d \n", sizeof (DATA32));
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
  cvCvtColor (gimg, g_gimg, CV_GRAY2RGB);

  /*
   * create a 32-bit grey:
   */
  IplImage* gimgf =  cvCreateImage (img_size, IPL_DEPTH_32F, 1); 
  if (g_clParams.smoothing_int == 0)
    cvConvertScale (gimg, gimgf, 1.0, 0.0);
  else
    smooth_image_float (gimg, gimgf, g_clParams.smoothing_int);

  /*
   * clean ups:
   */
  cvReleaseImage (&gimg);
  imlib_free_image ();
  ff_next_frame (videoP);

  return gimgf;
}

// static IplImage*
// LoadVideoFrameTo32BitGrey (CvCapture* cvCaptureP)
// {
//   IplImage* img = cvQueryFrame (cvCaptureP); // TC 2011-11-23 (Wed)
//                                              // --don't release this
//                                 // img, otherwise cvReleaseCapture will
//                                 // seg fault
//   /*
//    * Sanity checks:
//    */
//   if (unlikely (img == NULL))
//     {
//       // fprintf (stderr, "could not read video frame. End of file?\n");
//       return NULL;
//     }
//   g_cimg = cvCloneImage (img);


//   /*
//    * convert to 8-bit grey:
//    */
//   CvSize img_size = cvSize (img->width, img->height);
//   IplImage* gimg = cvCreateImage (img_size, IPL_DEPTH_8U, 1);
//   cvCvtColor (img, gimg, CV_RGB2GRAY);

//   /*
//    * create a 32-bit grey:
//    */
//   IplImage* gimgf =  cvCreateImage (img_size, IPL_DEPTH_32F, 1); 
//   cvConvertScale (gimg, gimgf, 1.0, 0.0);


//   /*
//    * clean ups:
//    */
//   cvReleaseImage (&gimg);

//   return gimgf;
// }

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
            basepart (g_clParams.videoFile_str));

  fprintf (stderr, "saving to file %s\n", fileName);
  cvSaveImage (fileName, imgP);
}



static void
Run ()
{
  /*-----------------------------
   * subsystems initialization:
   *---------------------------*/
  if (InitSystem () == false)
    ExitFailure ();

  /*-----------------------
   * controller loop:
   *---------------------*/
  // CvCapture* cvCaptureP = cvCreateFileCapture (g_clParams.videoFile_str);
  video_handle videoP = ff_open_video (g_clParams.videoFile_str);
  g_executing_flag = true;
  int frameIdx = 0;
  while (g_quit_int == 0)
    {
      /*
       * Step 1: Read the data:
       */
      // IplImage* gimgf = LoadVideoFrameTo32BitGrey (cvCaptureP);
      IplImage* gimgf = LoadVideoFrameTo32BitGrey (videoP);
      if (unlikely (gimgf == NULL))
        break;
      featurePool->AddImage (frameIdx, gimgf);
      // char name_str[80];
      // sprintf (name_str, "frame_%02d.jpg", frameIdx);
      // cvSaveImage (name_str, g_cimg);
      // sprintf (name_str, "greyframe_%02d.jpg", frameIdx);
      // cvSaveImage (name_str, gimgf);

      /*
       * Step 2: Track and select the features:
       */
      featurePool->Track_GPU (frameIdx, gimgf); 
      featurePool->Select_GPU (frameIdx, 
                               g_clParams.numFeatures_int,
                               g_clParams.numFeatures_int,
                               gimgf);
      featurePool->Purge (frameIdx, 10); 
      
      // /*
      //  * Step 3: Save to individual Log file:
      //  */
      // char logFileName_str[80];
      // snprintf (logFileName_str, sizeof(logFileName_str) - 1, 
      //           "output-%05d.klt", frameIdx);
      // fprintf (stderr, "saving %s\n", logFileName_str);
      // featurePool->SaveToFile_tc (logFileName_str);

      /*
       * Step 4: Overlay KTL result:
       */
      CvPoint p0, p1, q1;
      int scale = 1.0;
      int nFeatures =0;
      list<int> prevID = featurePool->GetActiveIDs (frameIdx-1);
      for (list<int>::iterator it = prevID.begin(); 
           it != prevID.end(); it++)
        {	
          CFeature2DTrack* pTrack = featurePool->GetFeatureTrackPtr (*it);
          if (pTrack == NULL || pTrack->empty()) 
            continue;
          
          CFeature2D* p_fp = pTrack->GetFeaturePtr (frameIdx-1);
          CFeature2D* c_fp = pTrack->GetFeaturePtr (frameIdx);
          
          p0.x = (int)(scale*(p_fp->m_x) + 0.5f);
          p0.y = (int)(scale*(p_fp->m_y) + 0.5f);
          p1.x = (int)(scale*(c_fp->m_x) + 0.5f);
          p1.y = (int)(scale*(c_fp->m_y) + 0.5f);
          
          q1.x = (int)(scale*(c_fp->m_x0) + 0.5f);
          q1.y = (int)(scale*(c_fp->m_y0) + 0.5f);
                    
          // Failure
          if (c_fp->m_status & CImageAlign::TRACK_FAIL) {
            cvCircle(g_gimg, p1, 1, CV_RGB(0,  0,  255), 2); // Blue
            // cvLine(g_gimg, p1, p0, CV_RGB(64, 64, 64), 1);
          }
          // Success
          else {
            cvLine (g_gimg, p1, p0, CV_RGB(0, 255, 0), 2);  // Green
            cvCircle (g_gimg, p1, 1, CV_RGB(255, 0, 0), 4); // Red
            nFeatures ++;
          }
        }
      fprintf (stderr, "nFeatures is %d\n", nFeatures);

      /*
       * Step 5: Display:
       */
      if (g_clParams.display_int) 
        {
          // cvShowImage (cvWindowName, g_cimg);
          cvShowImage (cvWindowName, g_gimg);
          char ch = (char) cvWaitKey (g_clParams.display_int);
          if (ch == 'q') 
            break;
        }

      /*
       * Step 6: Save Frames: (use feh to navigate through them)
       */
      if (g_clParams.saveFrames_int)
        SaveRendering (g_gimg, frameIdx);
      
      /*
       * Next frame
       */
      featurePool->SaveFrameResult_tc (); // needed for SaveAllToFile_tc
      frameIdx++;
    }

   /*
    * Step 3: Save all frame result to a single log file:
    */
  remove ("cuklt.text");     // make sure we start with clean state
  fprintf (stderr, "saving %d frames to cuklt.text\n", frameIdx);
  featurePool->SaveAllToFile_tc ("cuklt.text");
  fprintf (stderr, "cuklt.text saved\n");
  

  //CleanUps_lbl:
  // cvReleaseCapture (&cvCaptureP);
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
     "\n\nLast compiled on: %s\n"
     "Usage: %s [options] <jpgFile1> <jpgFile2>:\n"
     "[options] =\n"
     "  --help                Show this usage instruction.\n"
     "  --display <int>       Display the KLT; <int> = delay [ms] (eg 5)\n"
     "                        <int> = 0 will disable display (default)\n"
     "  --saveFrames          Save each individual frame jpg\n"
     "\n"
     "  --numFeatures  <nit>     <int> : features to detect (default 150)\n"
     "  --pyramid      <int>     <int> : levels in the pyramid (default 3)\n"
     "  --maskSize     <int>     <int> : ??? (default 15)\n"
     "  --templateSize <int>     <int> : ??? (default 11)\n"
     "  --smoothing    <int>     <int> : size of Gaussian kernel (default 3)\n"
     "                           set <int> = 0 to disable smoothing\n"
     "\n"
     "examples:\n"
     "  %s video.avi\n"
     "  %s --display 5 video.mov\n"
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
    {"help",            0, NULL,                      'h'},
    {"numFeatures",     1, NULL,                       0},
    {"pyramidLevels",   1, NULL,                       0},
    {"maskSize",        1, NULL,                       0},
    {"templateSize",    1, NULL,                       0},
    {"smoothing",       1, NULL,                       0},
    {"display",         1, NULL,                       0},
    {"saveFrames",      0, &g_clParams.saveFrames_int, 1},
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
          IsOpt_M ("numFeatures")
            g_clParams.numFeatures_int = atoi (optarg);
          IsOpt_M ("pyramidLevels")
            g_clParams.pyramidLevels_int = atoi (optarg);
          IsOpt_M ("maskSize")
            g_clParams.maskSize_int = atoi (optarg);
          IsOpt_M ("templateSize")
            g_clParams.templateSize_int = atoi (optarg);
          IsOpt_M ("smoothing")
            g_clParams.smoothing_int = atoi (optarg);
          IsOpt_M ("display")
            g_clParams.display_int = atoi (optarg);
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
  /* we are expecting a video files */
  if ((argc - optind) != 1)
    {
      fprintf (stderr, "expecting a video file\n");
      ExitFailure ();      
    }
  g_clParams.videoFile_str = argv[optind];
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


  // CvCapture* cvCaptureP = cvCreateFileCapture (g_clParams.videoFile_str);
  // fprintf (stderr, "tnum frames = %f\n",
  //          cvGetCaptureProperty (cvCaptureP, CV_CAP_PROP_FRAME_COUNT));
  // int frameIdx = 0;
  // while (g_quit_int == 0)
  //   {
  //     IplImage* img = cvQueryFrame (cvCaptureP); // TC 2011-11-23 (Wed)
  //     if (unlikely (img == NULL))
  //       break;
  //     frameIdx++;
  //   }
  // fprintf (stderr, "frameIdx is %d\n", frameIdx);
  // exit (0);

  // video_handle videoP = ff_open_video (g_clParams.videoFile_str);
  // if (videoP == NULL)
  //   exit (0);
  // int frameIdx = 0;
  // while (g_quit_int == 0)
  //   {
  //     if (ff_video_finished (videoP))
  //       break;
  //     // IplImage* gimgf = LoadVideoFrameTo32BitGrey (videoP);
  //     fprintf (stderr, "frameIdx = %d\n", frameIdx);
  //     ff_next_frame (videoP);
  //     frameIdx++;
  //   }
  // fprintf (stderr, "frameIdx is %d\n", frameIdx);
  // ff_close_and_free_video (videoP);
  // exit (0);

  /*-----------------------------
   * run system:
   *---------------------------*/
  Run ();


  /*-----------------------------
   * program termination:
   *---------------------------*/
  // fprintf (stderr, "Program Terminated Properly\n");
  return (EXIT_SUCCESS);
}
