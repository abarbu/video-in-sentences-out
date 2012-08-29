/**
 * @file cuklt.cpp
 *
 *  @remark ~ Implementation of CUDA KLT C-interface to Scheme
 *
 *  @author ~ Tommy Chang
 *
 *  @par Created by Tommy Chang, 2011-11-25 (Fri) 
 **/


/*----------------------------------*
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "featurePool.h"
#include "cuklt.h"
// #include <opencv/highgui.h>     // TBF only for dubugging cvSaveImage

/*---------------------------------------*
 * Global Static Variables and Constants *
 \*-------------------------------------*/
static int fprintf_count = 0;
static int g_cuklt_is_quiet = 0;
static FILE* null_device = NULL;

FILE* cuklt_stdmsg();

/*------------------*
 * Macro functions  *
 \*----------------*/
#if defined (__MINGW32__) || defined (__GNUC__) 
#  define fprintf(df, ...)                                      \
     do {                                                       \
       if (df == stderr)                                        \
         {                                                      \
           fprintf (stderr, "[cuklt %d]: ",                     \
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
class CukltClass
{
public:
  CukltClass ();
  ~CukltClass();
  bool Init (int imgWidth_int,
             int imgHeight_int,
             int maxFeatures_int,
             int nPyramidLevels_int,
             int maskSize_int,
             int templateSize_int,
             int smoothSize_int);

  int Detect (const uint32_t* pixels_argbA);
  int GetNframes () {return m_frameIdx;};
  bool SaveVideoKLT (const char* outFilePath_str);

  int           GetNresults (int frameIdx_int);
  const int*    GetIdresults (int frameIdx_int);
  const double* GetXresults  (int frameIdx_int);
  const double* GetYresults  (int frameIdx_int);

private:
  void SmoothImageFloat (const IplImage* gimg, // original 8-bit gray image
                         IplImage*       gimgf, // output 32-bit grey image
                         int             w);
  bool            m_initialized_flg;
  CFeature2DPool* m_featurePool;
  int             m_maxFeatures_int;
  IplImage*       m_cimg;
  int             m_frameIdx;
  int             m_smoothSize_int;
  
  int*            m_idA;
  double*         m_x_dblA;
  double*         m_y_dblA;  
};

CukltClass::
CukltClass () 
  : m_initialized_flg (false),
    m_featurePool (NULL), 
    m_maxFeatures_int (0),
    m_cimg (NULL), 
    m_frameIdx (0),
    m_smoothSize_int (0),
    m_idA (NULL),
    m_x_dblA (NULL),
    m_y_dblA (NULL)
{
  fprintf (cuklt_stdmsg(), "Calling %s\n", __FUNCTION__);
}

bool CukltClass::
Init (int imgWidth_int,
      int imgHeight_int,
      int maxFeatures_int,
      int nPyramidLevels_int,
      int maskSize_int,
      int templateSize_int,
      int smoothSize_int)
{
  fprintf (cuklt_stdmsg(), "Calling %s\n", __FUNCTION__);
  fprintf (cuklt_stdmsg(), "imgWidth_int = %d\n", imgWidth_int);
  fprintf (cuklt_stdmsg(), "imgHeight_int = %d\n", imgHeight_int);
  fprintf (cuklt_stdmsg(), "maxFeatures_int = %d\n", maxFeatures_int);
  fprintf (cuklt_stdmsg(), "nPyramidLevels_int = %d\n", nPyramidLevels_int);
  fprintf (cuklt_stdmsg(), "maskSize_int = %d\n", maskSize_int);
  fprintf (cuklt_stdmsg(), "templateSize_int = %d\n", templateSize_int);
  fprintf (cuklt_stdmsg(), "smoothSize_int = %d\n", smoothSize_int);

  /*
   * Don't init again if already initialized:
   */
  if (m_initialized_flg)
    {
      fprintf (cuklt_stdmsg(), "Already initialized, do nothing\n");
      return false;
    }

  /*
   * Create feature pool:
   */
  // If getting "setting the device when a process is active is not
  // allowed." It could be what the error says, you are trying to change
  // the device with cudaSetDevice() while you still have memory
  // allocated on the current GPU
  CvSize img_size = cvSize (imgWidth_int, imgHeight_int);
  fprintf (cuklt_stdmsg(), "imgWidth_int = %d imgHeight_int = %d\n",
           imgWidth_int, imgHeight_int);
  m_featurePool = new CFeature2DPool (img_size, 
                                      templateSize_int,
                                      nPyramidLevels_int, 
                                      maskSize_int, 
                                      true, NULL);
  fprintf (cuklt_stdmsg(), "done calling new CFeature2DPool()\n");
  if (m_featurePool == NULL)
    {
      fprintf (stderr, "could not allocate Feature Pool\n");
      return false;
    }

  /*
   * allocate C memory: 
   */
  if (m_maxFeatures_int > 1024)
    {
      fprintf (stderr, "number of features can not be greater than 1024\n");
      return false;
    }
  m_maxFeatures_int = maxFeatures_int;
  assert (sizeof (*m_idA) == sizeof (int));
  m_idA = (int*) malloc (m_maxFeatures_int * sizeof (int));
  memset (m_idA, 0, m_maxFeatures_int * sizeof (int));

  assert (sizeof (*m_x_dblA) == sizeof (double));
  m_x_dblA = (double*) malloc (m_maxFeatures_int * sizeof (double));

  assert (sizeof (*m_y_dblA) == sizeof (double));
  m_y_dblA = (double*) malloc (m_maxFeatures_int * sizeof (double));

  /*
   * Conversion to OpenCV image structure:
   */
  m_cimg = cvCreateImage (img_size, IPL_DEPTH_8U, 4);   
  m_smoothSize_int = smoothSize_int; // pre-smooth every frame

  m_initialized_flg = true;
  return true;                  // success
}

void CukltClass::
SmoothImageFloat (const IplImage* gimg, // original 8-bit gray image
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

int CukltClass::
Detect (const uint32_t* pixels_argbA)
{
  memcpy (m_cimg->imageData, pixels_argbA, 
          m_cimg->width * m_cimg->height * sizeof (uint32_t));
  
  /*
   * convert to 8-bit grey:
   */
  CvSize img_size = cvSize (m_cimg->width, m_cimg->height);
  IplImage* gimg = cvCreateImage (img_size, IPL_DEPTH_8U, 1);
  cvCvtColor (m_cimg, gimg, CV_RGBA2GRAY); // RGBA is misleading.  uint32
                                           // is always 0xAARRGGBB, but
                                           // in Little Endian the first
                                           // byte stored is B followed
                                           // by G, R, and A.
  // cvSaveImage("/tmp/test.pgm", gimg);


  /*
   * create a 32-bit grey:
   */
  IplImage* gimgf =  cvCreateImage (img_size, IPL_DEPTH_32F, 1); 
  if (m_smoothSize_int == 0)
    cvConvertScale (gimg, gimgf, 1.0, 0.0);
  else
    SmoothImageFloat (gimg, gimgf, m_smoothSize_int);
  cvReleaseImage (&gimg);

  /*
   * Step 1: Read the data:
   */
  if (unlikely (gimgf == NULL))
    return false;                  // failure
  m_featurePool->AddImage (m_frameIdx, gimgf);

  /*
   * Step 2: Track and select the features:
   */
  m_featurePool->Track_GPU (m_frameIdx, gimgf); 
  m_featurePool->Select_GPU (m_frameIdx, m_maxFeatures_int, 
                             m_maxFeatures_int, gimgf);
  m_featurePool->Purge (m_frameIdx, 10); 
  
  /*
   * Step 3: Next frame
   */
  int curFrameIdx = m_frameIdx;
  m_frameIdx++;
  m_featurePool->SaveFrameResult_tc (); // needed for SaveAllToFile_tc
  // TC 2012-01-03 (Tue) -- can't comment above out because that's where all
  // the data are stored for retrieval..

  return curFrameIdx;
}

CukltClass::
~CukltClass ()
{
  fprintf (cuklt_stdmsg(), "Calling %s\n", __FUNCTION__);
  free (m_idA);
  free (m_x_dblA);
  free (m_y_dblA);
  cvReleaseImage (&m_cimg);
  m_cimg = NULL;
  delete m_featurePool;
}

int CukltClass::
GetNresults (int frameIdx_int)
{
  assert (frameIdx_int <= m_frameIdx);
  vector<int> idVec = m_featurePool->GetFrameIdResult_tc (frameIdx_int);

  return idVec.size ();
}

const int* CukltClass::
GetIdresults (int frameIdx_int)
{
  assert (frameIdx_int <= m_frameIdx);
  vector<int> idVec = m_featurePool->GetFrameIdResult_tc (frameIdx_int);

  assert ((int) idVec.size () <= m_maxFeatures_int);
  memcpy (m_idA, &(idVec[0]), idVec.size () * sizeof (idVec[0]));

  return m_idA;
}

const double* CukltClass::
GetXresults (int frameIdx_int)
{
  assert (frameIdx_int <= m_frameIdx);
  vector<float> xVec = m_featurePool->GetFrameXresult_tc (frameIdx_int);
  for (unsigned int idx = 0; idx < xVec.size(); idx++)
    m_x_dblA[idx] = xVec[idx];
  // -- Is it necessary to convert to double?  Can scheme use float?
  return m_x_dblA;
}

const double* CukltClass::
GetYresults (int frameIdx_int)
{
  assert (frameIdx_int <= m_frameIdx);
  vector<float> yVec = m_featurePool->GetFrameYresult_tc (frameIdx_int);
  for (unsigned int idx = 0; idx < yVec.size(); idx++)
    m_y_dblA[idx] = yVec[idx];
  // -- Is it necessary to convert to double?  Can scheme use float?
  return m_y_dblA;
}

bool CukltClass::
SaveVideoKLT (const char* outFilePath_str)
{
  return m_featurePool->SaveAllToFile_tc (outFilePath_str);
}



/*------------------------*
 * Public Implementations *
 \*-----------------------*/

/** <!--cuklt_new()-->
 * @brief ~ Create a new KLT object.
 *
 *  @retval cuklt object pointer
 *
 *  @see ~ cuklt_delete()
 **/
void* 
cuklt_new ()
{
  fprintf (cuklt_stdmsg(), "Calling %s\n", __FUNCTION__);
  CukltClass *cukltObjP = new CukltClass();
  return cukltObjP;
}

/** <!--cuklt_delete()-->
 * @brief ~ Delete a created KLT object.
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *
 *  @see ~ cuklt_new()
 **/
void  
cuklt_delete (void* cuklt_ptr)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  delete cuklt_objP;
}

/** <!--cuklt_init_long()-->
 * @brief ~ Initialize the cuklt object (long version).
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *  @param[in] imgWidth_int ~ width of image
 *  @param[in] imgHeight_int ~ height of image
 *  @param[in] maxFeatures_int ~ max number of features (<1024) (eg 1000)
 *  @param[in] nPyramidLevels_int ~ number of pyramid levels (eg 3)
 *  @param[in] maskSize_int ~ ???? (eg 15)
 *  @param[in] templateSize_int ~ ???? (eg 11)
 *  @param[in] smoothSize_int ~ Size of Gaussian smoothing mask (eg 3)
 *
 *  @retval 0 ~ success
 *  @retval 1 ~ failure 
 *
 *  @see ~ cuklt_init()
 **/
int 
cuklt_init_long (void* cuklt_ptr,
                 int   imgWidth_int,
                 int   imgHeight_int,
                 int   maxFeatures_int,
                 int   nPyramidLevels_int,
                 int   maskSize_int,
                 int   templateSize_int,
                 int   smoothSize_int)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  if (cuklt_objP->Init (imgWidth_int, imgHeight_int, 
                        maxFeatures_int, nPyramidLevels_int,
                        maskSize_int, templateSize_int, smoothSize_int) == true)
    return 0; // success
  return 1;   // failure
}

/** <!--cuklt_init()-->
 * @brief ~ Initialize the cuklt object (short version).
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *  @param[in] imgWidth_int ~ width of image
 *  @param[in] imgHeight_int ~ height of image
 *
 *  @retval 0 ~ success
 *  @retval 1 ~ failure 
 *
 *  @see ~ cuklt_init_long()
 **/
int 
cuklt_init (void* cuklt_ptr,
            int   imgWidth_int,
            int   imgHeight_int)
{
  return cuklt_init_long (cuklt_ptr, imgWidth_int, imgHeight_int,
                          1000, 3, 15, 11, 3);
}

/** <!--cuklt_detect()-->
 * @brief ~ Perform KLT on the image data.
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *  @param[in] pixels_argbA ~ array of [BGRA] data
 *
 *  @retval the frame number
 *
 *  @see ~ cuklt_n_features(), cuklt_id_features()
 *  @see ~ cuklt_x_features(), cuklt_y_features()
 **/
int 
cuklt_detect (void*           cuklt_ptr,
              const uint32_t* pixels_argbA)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  return cuklt_objP->Detect (pixels_argbA);
}

/** <!--cuklt_n_frames()-->
 * @brief ~ Number of frames process so far.
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *
 *  @retval number of total frames so far
 *
 *  @see ~ cuklt_detect()
 **/
int 
cuklt_n_frames (void* cuklt_ptr)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  return cuklt_objP->GetNframes ();
}


/** <!--cuklt_n_features()-->
 * @brief ~ Get number of features detected in any processed frame.
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *  @param[in] frameIdx_int ~ which frame's result to get
 *
 *  @retval number of detected features
 *
 *  @see ~ cuklt_id_features(), cuklt_x_features(), cuklt_y_features(), 
 **/
int
cuklt_n_features (void* cuklt_ptr,
                  int   frameIdx_int)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  return cuklt_objP->GetNresults (frameIdx_int);
}

/** <!--cuklt_id_features()-->
 * @brief ~ Get the feature IDs from any of the previous processed frame.
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *  @param[in] frameIdx_int ~ which frame's result to get
 *
 *  @retval array of feature IDs
 *
 *  @see ~ cuklt_n_features(), cuklt_x_features(), cuklt_y_features(), 
 **/
const int*
cuklt_id_features (void* cuklt_ptr,
                   int   frameIdx_int)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  return cuklt_objP->GetIdresults (frameIdx_int);
}

/** <!--cuklt_x_features()-->
 * @brief ~ Get the X data from any of the previous processed frame.
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *  @param[in] frameIdx_int ~ which frame's result to get
 *
 *  @retval array of Y coordinates
 *
 *  @see ~ cuklt_n_features(), cuklt_y_features(), cuklt_id_features()
 **/
const double* 
cuklt_x_features (void* cuklt_ptr,
                  int   frameIdx_int)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  return cuklt_objP->GetXresults (frameIdx_int);
}

/** <!--cuklt_y_features()-->
 * @brief ~ Get the Y data from any of the previous processed frame.
 *
 *  @param[in] cuklt_ptr ~ cuklt object pointer
 *  @param[in] frameIdx_int ~ which frame's result to get
 *
 *  @retval array of Y coordinates
 *
 *  @see ~ cuklt_n_features(), cuklt_x_features(), cuklt_id_features()
 **/
const double* 
cuklt_y_features (void* cuklt_ptr,
                  int   frameIdx_int)
{
  CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  return cuklt_objP->GetYresults (frameIdx_int);
}

// Only errors are printed when in quiet mode
int 
cuklt_is_quiet()
{
  return g_cuklt_is_quiet;
}

void 
cuklt_set_quiet(int quiet)
{
  g_cuklt_is_quiet = quiet ? 1 : 0;
}

FILE* 
cuklt_stdmsg()
{
  if(cuklt_is_quiet()) {
    if(!null_device)
      null_device = fopen("/dev/null", "w");
    return null_device;
  }
  return stdout;
}


// /** <!--cuklt_save_per_video_klt()-->
//  * @brief ~ Save per-video data to klt.txt.
//  *
//  *  @param[in] cuklt_ptr ~ cuklt object pointer
//  *  @param[in] outputDir_str ~ name of the directory path to save under
//  *
//  *  @retval 0 ~ success
//  *  @retval 1 ~ failure 
//  *
//  *  @remark ~ Call this function to save all processed frames so far.  You
//  *  only need to call it at the end of all detections.  Don't call this
//  *  function every frame.
//  **/
// int 
// cuklt_save_per_video_klt (void*       cuklt_ptr, 
//                           const char* outputDir_str)
// {
//   CukltClass* cuklt_objP = (CukltClass*) cuklt_ptr;
  
//   if (cuklt_objP->SaveVideoKLT (outputDir_str) == true)
//     return 0;                     // success
//   return 1;                       // failure
// }
