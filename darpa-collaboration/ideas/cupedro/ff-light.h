
#pragma once

/** ----------------------------------------------------------------------- Example
 * Opens a video file, and then write each frame to /tmp
 */
/*
void vid_test(const char* filename)
{
  video_handle video = ff_open_video(filename);

  if(video) {
    int frame_no = 0;
    char buffer[1024];

    while(!ff_video_finished(video)) {
	printf("Reading frame %04d\n", frame_no);

	Imlib_Image* handle = ff_get_frame_as_imlib(video);
	if(handle) {
	  sprintf(buffer, "/tmp/zz-frame-%04d.png", frame_no);
	  imlib_context_set_image(handle);
	  imlib_save_image(buffer);
	  imlib_free_image();
	}
	
	ff_next_frame(video);
	frame_no++;
    }

    ff_close_and_free_video(video);
  }
}
*/


#include <stdint.h>
#include <Imlib2.h>

// ------------------------------------------------------------------------- Predefines
#ifdef __cplusplus
extern "C" {
#endif

  typedef void* video_handle;

  /** Open/close an video file */
  video_handle ff_open_video(const char* filename);
  void ff_close_and_free_video(video_handle video);
  
  /** Width and height of video */
  unsigned int ff_video_width(video_handle video);
  unsigned int ff_video_height(video_handle video);

  /** Reads in the next frame */
  void ff_next_frame(video_handle video);

  /** Returns the buffer size necessary to contain a single frame */
  unsigned int ff_required_buffer_size(video_handle video);

  /** Getting access to the decoded frame byte data 
   * Use ff_required_buffer_size to set the correct buffer size
   */
  void ff_get_frame(video_handle video, uint8_t* buffer);
  Imlib_Image* ff_get_frame_as_imlib(video_handle video);

  /** TRUE iff next-frame has been called for every frame */
  char ff_video_finished(video_handle video);  

#ifdef __cplusplus
}

/* c++ interface here */


#endif


