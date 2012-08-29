
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>

#include "ff-light.h"

struct ffmpeg_video_t {
  AVFormatContext *pFormatCtx;
  int videoStream;
  AVCodecContext *pCodecCtx;
  AVFrame *pFrame;
  AVFrame *pFrameBGRA;
  uint8_t *buffer;
  struct SwsContext *img_convert_ctx;
  AVPacket packet;
  int frame;
  int videoFinished;
};

#define FFVIDEO(video) ((struct ffmpeg_video_t*) video)

/** ----------------------------------------------------------------- predefines -- */
int ff_first_video_stream(video_handle video);
AVCodecContext* ff_get_codec(video_handle video);

/** --------------------------------------------------------------------- decode_video
 * avcodec_decode_video has been deprecated, and does not appear in recent
 * versions of ffmpeg -- this is the appropriate wrapper function
 */
int decode_video(AVCodecContext *avctx, AVFrame *picture,
							   int *got_picture_ptr,
							   const uint8_t *buf, int buf_size)
{
  AVPacket avpkt;
  av_init_packet(&avpkt);
  avpkt.data = (uint8_t*) buf;
  avpkt.size = buf_size;
  // HACK for CorePNG to decode as normal PNG by default
  avpkt.flags = AV_PKT_FLAG_KEY;
 
  return avcodec_decode_video2(avctx, picture, got_picture_ptr, &avpkt);
}

// ----------------------------------------------------------- ffmpeg_first_video_stream
int ff_first_video_stream(video_handle video) 
{
  if(av_find_stream_info(FFVIDEO(video)->pFormatCtx)<0) {
    fprintf(stderr, "error: Can't get video stream information\n");
    exit(-1);
  }
  unsigned int i;       /* TC 2011-11-30 (Wed) -- compiler wasn't happy*/
  for(i=0; i < FFVIDEO(video)->pFormatCtx->nb_streams; i++) {
    if(FFVIDEO(video)->pFormatCtx->streams[i]->codec->codec_type==CODEC_TYPE_VIDEO) {
      return i;
    }
  }
  fprintf(stderr, "error: Can't find first video stream");
  exit(-1);
}

// --------------------------------------------------------------------- ff_get_codec
AVCodecContext* ff_get_codec(video_handle video) 
{
  AVCodecContext *pCodecCtx = FFVIDEO(video)->pFormatCtx->streams[FFVIDEO(video)->videoStream]->codec;
  AVCodec *pCodec = avcodec_find_decoder(pCodecCtx->codec_id);
  if(pCodec==NULL) {
    fprintf(stderr,"error: Unsupported codec!");
    exit(-1);
  }
  if(avcodec_open(pCodecCtx, pCodec)<0) {
    fprintf(stderr,"error: Can't open codec!");
    exit(-1);
  }
  return pCodecCtx;
}

// ------------------------------------------------------------------- ff_next_frame
void ff_next_frame(video_handle video) 
{
  if(FFVIDEO(video)->videoFinished == 2) {
    FFVIDEO(video)->videoFinished = 1;
  } else {
    av_free_packet(&FFVIDEO(video)->packet);
    int frameFinished;
    int nextFrameValid = av_read_frame(FFVIDEO(video)->pFormatCtx, &FFVIDEO(video)->packet) >= 0;
    if(nextFrameValid && FFVIDEO(video)->packet.stream_index==FFVIDEO(video)->videoStream) {
      decode_video(FFVIDEO(video)->pCodecCtx, FFVIDEO(video)->pFrame, &frameFinished,
				   FFVIDEO(video)->packet.data, FFVIDEO(video)->packet.size);
      if(frameFinished) FFVIDEO(video)->frame++;
      else ff_next_frame(video);
    } else if(nextFrameValid) {
      ff_next_frame(video);
    } else if(!FFVIDEO(video)->videoFinished && !nextFrameValid) {
      // This is required because ffmpeg hangs on to one frame internally
      // so we need to read one past the end of the video
      FFVIDEO(video)->videoFinished = 2;
      decode_video(FFVIDEO(video)->pCodecCtx, FFVIDEO(video)->pFrame, &frameFinished, 0, 0);
      FFVIDEO(video)->frame++;
    }
  }
}

// ---------------------------------------------------------------- ff_video_finished
char ff_video_finished(video_handle video) 
{
  return FFVIDEO(video)->videoFinished == 1;
}

// ---------------------------------------------------------- ff_close_and_free_video
void ff_close_and_free_video(video_handle video) 
{
  av_free(FFVIDEO(video)->buffer);
  av_free(FFVIDEO(video)->pFrameBGRA);
  av_free(FFVIDEO(video)->pFrame);
  avcodec_close(FFVIDEO(video)->pCodecCtx);
  av_close_input_file(FFVIDEO(video)->pFormatCtx);
  av_free_packet(&FFVIDEO(video)->packet);
  FFVIDEO(video)->videoFinished = 1;
  free(video);
}

// -------------------------------------------------------------------- ff_open_video
video_handle ff_open_video(const char* filename) 
{
  video_handle video = (video_handle) malloc(sizeof(struct ffmpeg_video_t));
  bzero(video, sizeof(struct ffmpeg_video_t));
  av_register_all();
  /* if(av_open_input_file(&FFVIDEO(video)->pFormatCtx, filename, NULL, 0, NULL)!=0) { */
  // TC 2011-12-30 (Fri) -- make compiler happy
  if (avformat_open_input (&FFVIDEO(video)->pFormatCtx, 
                           filename, NULL, NULL)!=0) {
    fprintf(stderr, "error: Can't open video\n");
    free(video);
    return NULL;
  }
  FFVIDEO(video)->videoStream = ff_first_video_stream(video);
  FFVIDEO(video)->pCodecCtx = ff_get_codec(video);
  FFVIDEO(video)->pFrame = avcodec_alloc_frame();
  FFVIDEO(video)->pFrameBGRA = avcodec_alloc_frame();
  if(!FFVIDEO(video)->pFrameBGRA || !FFVIDEO(video)->pFrame) {
    fprintf(stderr,"error: Can't allocate frame!");
    avcodec_close(FFVIDEO(video)->pCodecCtx);
    free(video);
    return NULL;
  }
  FFVIDEO(video)->buffer =
    (uint8_t *)av_malloc(avpicture_get_size(PIX_FMT_BGRA,
							  FFVIDEO(video)->pCodecCtx->width,
							  FFVIDEO(video)->pCodecCtx->height) *
				 sizeof(uint8_t));
  avpicture_fill((AVPicture *)FFVIDEO(video)->pFrameBGRA, FFVIDEO(video)->buffer, PIX_FMT_BGRA,
		     FFVIDEO(video)->pCodecCtx->width, FFVIDEO(video)->pCodecCtx->height);
  FFVIDEO(video)->img_convert_ctx =
    sws_getContext(FFVIDEO(video)->pCodecCtx->width, FFVIDEO(video)->pCodecCtx->height,
			 FFVIDEO(video)->pCodecCtx->pix_fmt,
			 FFVIDEO(video)->pCodecCtx->width, FFVIDEO(video)->pCodecCtx->height,
			 PIX_FMT_BGRA, SWS_BICUBIC,
			 NULL, NULL, NULL);
  FFVIDEO(video)->videoFinished = 0;
  FFVIDEO(video)->frame = 0;
  av_init_packet(&FFVIDEO(video)->packet);
  ff_next_frame(video);
  return video;
}

// --------------------------------------------------------  ff_required_buffer_size
unsigned int ff_required_buffer_size(video_handle video)
{
  return sizeof(uint8_t) * avpicture_get_size(PIX_FMT_BGRA,
							    FFVIDEO(video)->pCodecCtx->width,
							    FFVIDEO(video)->pCodecCtx->height);
}
;
// -------------------------------------------------------------------- ff_get_frame
void ff_get_frame(video_handle video, uint8_t* buffer)
{
  sws_scale(FFVIDEO(video)->img_convert_ctx, (const uint8_t * const*)FFVIDEO(video)->pFrame->data,
		FFVIDEO(video)->pFrame->linesize, 0,
		FFVIDEO(video)->pCodecCtx->height,
		FFVIDEO(video)->pFrameBGRA->data, FFVIDEO(video)->pFrameBGRA->linesize);
 
  memcpy(buffer, FFVIDEO(video)->buffer, ff_required_buffer_size(video));
}

// ------------------------------------------------------------------ ff_video_width
unsigned int ff_video_width(video_handle video) 
{
  return FFVIDEO(video)->pCodecCtx->width;
}

// ----------------------------------------------------------------- ff_video_height
unsigned int ff_video_height(video_handle video) 
{
  return FFVIDEO(video)->pCodecCtx->height;
}

// ----------------------------------------------------------- ff_get_frame_as_imlib
Imlib_Image *ff_get_frame_as_imlib(video_handle video) 
{
  sws_scale(FFVIDEO(video)->img_convert_ctx, (const uint8_t * const*)FFVIDEO(video)->pFrame->data,
		FFVIDEO(video)->pFrame->linesize, 0,
		FFVIDEO(video)->pCodecCtx->height,
		FFVIDEO(video)->pFrameBGRA->data, FFVIDEO(video)->pFrameBGRA->linesize);
  Imlib_Image *image =
    imlib_create_image_using_copied_data(FFVIDEO(video)->pCodecCtx->width,
						     FFVIDEO(video)->pCodecCtx->height,
						     (uint32_t*)FFVIDEO(video)->buffer);
  return image;
}


