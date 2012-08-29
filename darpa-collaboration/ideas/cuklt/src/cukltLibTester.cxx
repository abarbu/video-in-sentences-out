/**
 * @file cukltTester.cpp
 *
 *  @remark ~ Test the libcuklt library.
 *
 *  @author ~ Tommy Chang
 *
 *  @par Created by Tommy Chang, 2011-12-02 (Fri) 
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

#include "cuklt.h"
#include <cv.h>
#include <opencv/highgui.h>

typedef struct {
  int         debugLevel_bitored;
  const char *statusFile_str;
  const char *jpgFile1_str;
  const char *jpgFile2_str;
  int         numFeatures_int;
  int         pyramidLevels_int;
  int         maskSize_int;
  int         templateSize_int;
  int         smoothSize_int;
} CommandLineParamType;



/*---------------------------------------*
 * Global Static Variables and Constants *
 \*-------------------------------------*/
static int fprintf_count = 0;
static CommandLineParamType g_clParams = {
  0,                            // debugLevel_bitored
  NULL,                         // statusFile_str
  NULL,                         // jpgFile1_str
  NULL,                         // jpgFile2_str
  150,                          // numFeatures_int
  3,                            // pyramidLevels_int
  15,                           // maskSize_int
  11,                           // templateSize_int
  3,                            // smoothSize_int
};
static int  g_quit_int = 0;
static bool g_executing_flag = false;
static void *g_cuklt_objP = NULL;


/*------------------*
 * Macro functions  *
 \*----------------*/
#if defined (__MINGW32__) || defined (__GNUC__) 
#  define fprintf(df, ...)                                      \
     do {                                                       \
       if (df == stderr)                                        \
         {                                                      \
           fprintf (stderr, "[cukltTester %d]: ",               \
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
  cuklt_delete (g_cuklt_objP); 
  g_cuklt_objP = NULL;
  fprintf (stderr, "Done CleanUps\n");
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
GetImageSizeFromFile (const char *fileName_str,
                      int        *imgWidth_intP,
                      int        *imgHeight_intP)
{
  IplImage* img = cvLoadImage (fileName_str);
  if (img == NULL)
    {
      fprintf (stderr, "could not read %s\n", fileName_str);
      return false;
    }

  if (imgWidth_intP)
    *imgWidth_intP = img->width;
  if (imgHeight_intP)
    *imgHeight_intP = img->height;
  
  return true;
}


static bool
InitSystem ()
{
  /*
   * Get image dimension:
   */
  int imgWidth_int, imgHeight_int;
  if (GetImageSizeFromFile (g_clParams.jpgFile1_str, 
                            &imgWidth_int, &imgHeight_int) == false)
    {
      fprintf (stderr, "[Error]: could not open image file '%s'\n",
               g_clParams.jpgFile1_str);
      return false;
    }


  /*
   * Create klt object using c-interface:
   */
  g_cuklt_objP = cuklt_new ();
  if (0 == cuklt_init_long (g_cuklt_objP,
                            imgWidth_int,
                            imgHeight_int,
                            g_clParams.numFeatures_int,
                            g_clParams.pyramidLevels_int,
                            g_clParams.maskSize_int,
                            g_clParams.templateSize_int,
                            g_clParams.smoothSize_int))
    return true;
  return false;
}


static void 
SaveToFile_tc (int         frameIdx,
               const char* fname)
{
  FILE *outFileP = fopen (fname, "wb");
  
  int nFeatures =  cuklt_n_features (g_cuklt_objP, frameIdx);
  const int* ID_intA =  cuklt_id_features (g_cuklt_objP, frameIdx);
  const double* X_dblA  = cuklt_x_features (g_cuklt_objP, frameIdx);
  const double* Y_dblA  = cuklt_y_features (g_cuklt_objP, frameIdx);
  
  /*
   * Step 1: write number of features:
   */
  fprintf (outFileP, "nFeatures_v2 = %d\n", nFeatures);

  /*
   * Step 2: write features:
   */
  for (int idx = 0; idx < nFeatures; idx++)
    {
      fprintf (outFileP, "%d %.2f %.2f\n", ID_intA[idx],
               X_dblA[idx], Y_dblA[idx]);
    }

  fclose (outFileP);
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

  /*-----------------------
   * controller loop:
   *---------------------*/
  g_executing_flag = true;
  const char *jpgFileNames_strA[2] = {g_clParams.jpgFile1_str, 
                                      g_clParams.jpgFile2_str};
  const char *logFileNames_strA[2] = {"klt-current.klt", "klt-next.klt"};
  int frameIdx = 0;
  while ((g_quit_int == 0)
         && (frameIdx < 2))       // per-frame-klt takes just 2 frames
    {
      /*
       * Step 1: Read the data:
       */
      IplImage* img24 = cvLoadImage (jpgFileNames_strA[frameIdx]);
      if (img24 == NULL)
        {
          fprintf (stderr, "could not read %s\n", 
                   jpgFileNames_strA[frameIdx]);
          ExitFailure ();
        }
      CvSize img_size = cvSize (img24->width, img24->height);
      IplImage* img32 =  cvCreateImage (img_size, IPL_DEPTH_8U, 4); 
      cvCvtColor (img24, img32, CV_RGB2RGBA);
      // uint32_t *raw = (uint32_t*) (img32->imageData);
      // unsigned char* charP = (unsigned char*) &(raw[500]);
      // fprintf (stderr, "char[0],char[1],char[2],char[3] is %X, %X, %X, %X\n",
      //          charP[0], charP[1], charP[2], charP[3]);
      // cvSaveImage ("delme.jpg", img32);

      /*
       * Step 2: Run KLT detection:
       */
      cuklt_detect (g_cuklt_objP, (const uint32_t*) (img32->imageData));

      /*
       * Step 3: Save to Log file:
       */
      fprintf (stderr, "saving %s\n", logFileNames_strA[frameIdx]);
      SaveToFile_tc (frameIdx, logFileNames_strA[frameIdx]);

      /*
       * Next frame:
       */
      cvReleaseImage (&img24);  // TBF
      cvReleaseImage (&img32);  // TBF
      frameIdx ++;
    }

  // /*
  //  *  Save the video KTL file
  //  */
  // cuklt_save_per_video_klt (g_cuklt_objP, "./klt.text");
  // TC 2012-01-02 (Mon) -- No longer save all klt pairs

  //CleanUps_lbl:
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
     "Usage: %s [options]:\n"
     "[options] =\n"
     "  --help                Show this usage instruction.\n"
     "  --debug <int>         <int> : level of verboseness;\n" 
     "                            can be multi-valued; default 0.\n" 
     "  --statusFile <str>    <str> : output status file.\n" 
     "                            The status file contains the pid \n" 
     "                            if success, otherwise 0.\n" 
     "  --                    Allow no option supplied -- no effect.\n"
     "\n"
     "  --numFeatures  <nit>     <int> : features to detect (default 150)\n"
     "  --pyramid      <int>     <int> : levels in the pyramid (default 3)\n"
     "  --maskSize     <int>     <int> : ??? (default 15)\n"
     "  --templateSize <int>     <int>: ??? (default 11)\n"
     "  --smoothSize   <int>     <int>: Smooth kernel size (default 3)\n"
     "\n"
     "examples:\n"
     "  %s file1.jpg file2.jpg\n"
     "\n", __DATE__
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
    {"numFeatures",     1, NULL,        0},
    {"pyramidLevels",   1, NULL,        0},
    {"maskSize",        1, NULL,        0},
    {"templateSize",    1, NULL,        0},
    {"smoothSize",      1, NULL,        0},
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
          IsOpt_M ("numFeatures")
            g_clParams.numFeatures_int = atoi (optarg);
          IsOpt_M ("pyramidLevels")
            g_clParams.pyramidLevels_int = atoi (optarg);
          IsOpt_M ("maskSize")
            g_clParams.maskSize_int = atoi (optarg);
          IsOpt_M ("templateSize")
            g_clParams.templateSize_int = atoi (optarg);
          IsOpt_M ("smoothSize")
            g_clParams.templateSize_int = atoi (optarg);
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
  /* we are expecting 2 jpg files */
  if ((argc - optind) != 2)
    {
      fprintf (stderr, "expecting 2 jpg files\n");
      ExitFailure ();      
    }
  g_clParams.jpgFile1_str = argv[optind];
  g_clParams.jpgFile2_str = argv[optind+1];
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
  for (int idx = 0; idx < 10; idx++)
    {
      fprintf (stderr, "----------------------------------\n");
      fprintf (stderr, "------------- Iteration Test %d\n", idx);
      fprintf (stderr, "----------------------------------\n");
      Run ();
      fprintf (stderr, "\n\n");
    }



  /*-----------------------------
   * program termination:
   *---------------------------*/
  fprintf (stderr, "Program Terminated Properly\n");
  return (EXIT_SUCCESS);
}
