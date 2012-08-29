/**
 * @file cuklt.h
 *
 *  @remark ~ Interface of CUDA KLT C-interface to Scheme
 *
 *  @author ~ Tommy Chang
 *
 *  @par Created by Tommy Chang, 2011-11-25 (Fri) 
 **/


#ifndef CUKLT_H
#define CUKLT_H


/*----------------------------------*
 * Preprocessor and Include Headers *
 \*--------------------------------*/
#include <stdint.h>

/*-----------------*
 * Macro Constants *
 \*---------------*/



/*-----------------*
 * Data Structures *
 \*---------------*/



/*--------------------*
 * Function Prototype *
 \*------------------*/
#ifdef __cplusplus
extern "C" {
#endif
  // Create a new klt object that runs on CUDA hardware:
  void* cuklt_new (); 
  void  cuklt_delete (void* cuklt_objP); 
  
  // Initialize the klt object:
  int cuklt_init (void* cuklt_objP,
                  int   imgWidth_int,
                  int   imgHeight_int);
  int cuklt_init_long (void* cuklt_objP,
                       int   imgWidth_int,
                       int   imgHeight_int,
                       int   maxFeatures_int,    /* eg. 1000 */
                       int   nPyramidLevels_int, /* eg. 3 */
                       int   maskSize_int,       /* eg. 15 */
                       int   templateSize_int,   /* eg. 11 */
                       int   smoothSize_int);    /* eg. 0, 3, 13, 23, 33, ... */

  // Only errors are printed when in quiet mode
  int cuklt_is_quiet();
  void cuklt_set_quiet(int quiet);

  // Process a single image/frame:
  int cuklt_detect (void*           cuklt_objP,
                    const uint32_t* pixels_argbA);
  int cuklt_n_frames (void* cuklt_objP);

  // Pick up any results from previously processed frame:
  int           cuklt_n_features  (void* cuklt_objP,
                                   int   frameIdx_int);
  const int*    cuklt_id_features (void* cuklt_objP,
                                   int   frameIdx_int);
  const double* cuklt_x_features  (void* cuklt_objP,
                                   int   frameIdx_int);
  const double* cuklt_y_features  (void* cuklt_objP,
                                   int   frameIdx_int);
                             
  /* // Call this function to save all the frames process so far to a file; */
  /* // it will overwrite and create a new file. */
  /* int cuklt_save_per_video_klt (void*       cuklt_objP,  */
  /*                               const char* outputFile_str); */
  // TC 2012-01-02 (Mon) -- No longer save all klt pairs

#ifdef __cplusplus
}
#endif


#endif // CUKLT_H
