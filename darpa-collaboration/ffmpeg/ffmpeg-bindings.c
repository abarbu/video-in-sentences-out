#include <strings.h>
#include "ffmpeg-bindings.h"

int ffmpeg_first_video_stream(struct ffmpeg_video_t *video) {
  if(av_find_stream_info(video->formatContext)<0) {
    fprintf(stderr, "error: Can't get video stream information\n");
    exit(-1);
  }
  for(unsigned int i=0; i < video->formatContext->nb_streams; i++)
    if(video->formatContext->streams[i]->codec->codec_type==CODEC_TYPE_VIDEO)
      return i;
  fprintf(stderr, "error: Can't find first video stream");
  exit(-1);
}

AVCodecContext *ffmpeg_get_codec(struct ffmpeg_video_t *video) {
  AVCodecContext *codecContext = video->formatContext->streams[video->videoStream]->codec;
  AVCodec *pCodec = avcodec_find_decoder(codecContext->codec_id);
  if(pCodec==NULL) {
    fprintf(stderr,"error: Unsupported codec!");
    exit(-1);
  }
  if(avcodec_open(codecContext, pCodec)<0) {
    fprintf(stderr,"error: Can't open codec!");
    exit(-1);
  }
  return codecContext;
}

void ffmpeg_next_frame(struct ffmpeg_video_t *video) {
  if(video->videoFinished == 2) {
    video->videoFinished = 1;
  } else {
    av_free_packet(&video->packet);
    int frameFinished;
    int nextFrameValid = av_read_frame(video->formatContext, &video->packet) >= 0;
    if(nextFrameValid && video->packet.stream_index==video->videoStream) {
      avcodec_decode_video(video->codecContext, video->frame, &frameFinished,
			   video->packet.data, video->packet.size);
      if(frameFinished) video->frameNumber++;
      else ffmpeg_next_frame(video);
    } else if(nextFrameValid) {
      ffmpeg_next_frame(video);
    } else if(!video->videoFinished && !nextFrameValid) {
      // This is required because ffmpeg hangs on to one frame internally
      // so we need to read one past the end of the video
      video->videoFinished = 2;
      avcodec_decode_video(video->codecContext, video->frame, &frameFinished, 0, 0);
      video->frameNumber++;
    }
  }
}

char ffmpeg_video_finished(struct ffmpeg_video_t *video) {
  return video->videoFinished == 1;
}

void ffmpeg_close_and_free_video(struct ffmpeg_video_t *video) {
  av_free(video->buffer);
  av_free(video->frameDecoded);
  av_free(video->frame);
  avcodec_close(video->codecContext);
  av_close_input_file(video->formatContext);
  av_free_packet(&video->packet);
  video->videoFinished = 1;
  free(video);
}

struct ffmpeg_video_t *ffmpeg_open_video(const char* filename, enum PixelFormat format) {
  struct ffmpeg_video_t *video = malloc(sizeof(struct ffmpeg_video_t));
  bzero(video, sizeof(video));
  av_register_all();
  if(av_open_input_file(&video->formatContext, filename, NULL, 0, NULL)!=0) {
    fprintf(stderr, "error: Can't open video\n");
    free(video);
    return NULL;
  }
  video->format = format;
  video->videoStream = ffmpeg_first_video_stream(video);
  video->codecContext = ffmpeg_get_codec(video);
  video->frame = avcodec_alloc_frame();
  video->frameDecoded = avcodec_alloc_frame();
  if(!video->frameDecoded || !video->frame) {
    fprintf(stderr,"error: Can't allocate frame!");
    avcodec_close(video->codecContext);
    free(video);
    return NULL;
  }
  video->buffer =
    (uint8_t *)av_malloc(avpicture_get_size(video->format,
					    video->codecContext->width,
					    video->codecContext->height) *
			 sizeof(uint8_t));
  avpicture_fill((AVPicture *)video->frameDecoded, video->buffer, video->format,
		 video->codecContext->width, video->codecContext->height);
  video->convertContext =
    sws_getContext(video->codecContext->width, video->codecContext->height,
		   video->codecContext->pix_fmt,
		   video->codecContext->width, video->codecContext->height,
		   video->format, SWS_BICUBIC, NULL, NULL, NULL);
  video->videoFinished = 0;
  video->frameNumber = 0;
  av_init_packet(&video->packet);
  ffmpeg_next_frame(video);
  return video;
}

uint8_t *ffmpeg_get_frame(struct ffmpeg_video_t *video) {
  uint8_t *data = malloc(avpicture_get_size(video->format,
					    video->codecContext->width,
					    video->codecContext->height) * sizeof(uint8_t));
  sws_scale(video->convertContext, (const uint8_t * const*)video->frame->data,
	    video->frame->linesize, 0,
	    video->codecContext->height,
	    video->frameDecoded->data, video->frameDecoded->linesize);
  memcpy(data, video->buffer,
	 avpicture_get_size(video->format,
			    video->codecContext->width,
			    video->codecContext->height) * sizeof(uint8_t));
  return data;
}

unsigned int ffmpeg_video_width(struct ffmpeg_video_t *video) {
  return video->codecContext->width;
}

unsigned int ffmpeg_video_height(struct ffmpeg_video_t *video) {
  return video->codecContext->height;
}

Imlib_Image *ffmpeg_get_frame_as_imlib(struct ffmpeg_video_t *video) {
  sws_scale(video->convertContext, (const uint8_t * const*)video->frame->data,
	    video->frame->linesize, 0,
	    video->codecContext->height,
	    video->frameDecoded->data, video->frameDecoded->linesize);
  Imlib_Image *image =
    imlib_create_image_using_copied_data(video->codecContext->width,
					 video->codecContext->height,
					 (uint32_t*)video->buffer);
  return image;
}
