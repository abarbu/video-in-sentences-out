/**
 * @file per-frame-klt.cxx
 *
 *  @remark ~ Per-Frame KLT using CUDA library.
 *
 *  @author ~ Tommy Chang
 *
 *  @par Created by Tommy Chang, 2011-11-21 (Mon) 
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

#include "featurePool.h"
#include <opencv/highgui.h>

typedef struct {
  const char *jpgFile1_str;
  const char *jpgFile2_str;
  int         numFeatures_int;
  int         pyramidLevels_int;
  int         maskSize_int;
  int         templateSize_int;
} CommandLineParamType;


/*---------------------------------------*
 * Global Static Variables and Constants *
 \*-------------------------------------*/
static CFeature2DPool* featurePool = NULL;
static int fprintf_count = 0;
static CommandLineParamType g_clParams = {
  NULL,                         // jpgFile1_str
  NULL,                         // jpgFile2_str
  150,                          // numFeatures_int
  3,                            // pyramidLevels_int
  15,                           // maskSize_int
  11,                           // templateSize_int
};
static int  g_quit_int = 0;
static bool g_executing_flag = false;


/*------------------*
 * Macro functions  *
 \*----------------*/
#if defined (__MINGW32__) || defined (__GNUC__) 
#  define fprintf(df, ...)                                      \
     do {                                                       \
       if (df == stderr)                                        \
         {                                                      \
           fprintf (stderr, "[per-frame-klt %d]: ",             \
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
   * Create feature pool:
   */
  CvSize img_size = cvSize (imgWidth_int, imgHeight_int);
  featurePool = new CFeature2DPool (img_size, 
                                    g_clParams.templateSize_int, 
                                    g_clParams.pyramidLevels_int, 
                                    g_clParams.maskSize_int, 
                                    true, NULL);

  return true;
}

static IplImage*
LoadJpgFileTo32BitGrey (const char *fileName_str)
{
  IplImage* img = cvLoadImage (fileName_str);
  if (img == NULL)
    {
      fprintf (stderr, "could not read %s\n", fileName_str);
      ExitFailure ();
    }

  // convert to 8-bit grey:
  CvSize img_size = cvSize (img->width, img->height);
  IplImage* gimg = cvCreateImage (img_size, IPL_DEPTH_8U, 1);
  cvCvtColor (img, gimg, CV_RGB2GRAY);

  // create a 32-bit grey:
  IplImage* gimgf =  cvCreateImage (img_size, IPL_DEPTH_32F, 1); 
  cvConvertScale (gimg, gimgf, 1.0, 0.0);

  // clean ups:
  cvReleaseImage (&gimg);
  cvReleaseImage (&img);

  return gimgf;
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
      IplImage* gimgf = LoadJpgFileTo32BitGrey (jpgFileNames_strA[frameIdx]);
      featurePool->AddImage(frameIdx, gimgf); 


      /*
       * Step 2: Track and select the features:
       */
      featurePool->Track_GPU (frameIdx, gimgf); 
      featurePool->Select_GPU (frameIdx, 
                               g_clParams.numFeatures_int, // min features
                               g_clParams.numFeatures_int, // max features
                               gimgf);
      featurePool->Purge (frameIdx, 10); 
      

      /*
       * Step 3: Save to Log file:
       */
      fprintf (stderr, "saving %s\n", logFileNames_strA[frameIdx]);
      featurePool->SaveToFile_tc (logFileNames_strA[frameIdx]);

      /*
       * Next frame
       */
      frameIdx++;
    }


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
     "\n\nLast compiled on: %s\n"
     "Usage: %s [options] <jpgFile1> <jpgFile2>:\n"
     "[options] =\n"
     "  --help                Show this usage instruction.\n"
     "\n"
     "  --numFeatures  <nit>     <int> : features to detect (default 150)\n"
     "  --pyramid      <int>     <int> : levels in the pyramid (default 3)\n"
     "  --maskSize     <int>     <int> : ??? (default 15)\n"
     "  --templateSize <int>     <inst>: ??? (default 11)\n"
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
    {"help",            0, NULL,       'h'},
    {"numFeatures",     1, NULL,        0},
    {"pyramidLevels",   1, NULL,        0},
    {"maskSize",        1, NULL,        0},
    {"templateSize",    1, NULL,        0},
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
  Run ();


  /*-----------------------------
   * program termination:
   *---------------------------*/
  // fprintf (stderr, "Program Terminated Properly\n");
  return (EXIT_SUCCESS);
}
