#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
#include <libavutil/avutil.h>
#include <Imlib2.h>

struct ffmpeg_video_t {
  AVFormatContext *formatContext;
  int videoStream;
  enum PixelFormat format;
  AVCodecContext *codecContext;
  AVFrame *frame;
  AVFrame *frameDecoded;
  uint8_t *buffer;
  struct SwsContext *convertContext;
  AVPacket packet;
  int frameNumber;
  int videoFinished;
};

uint8_t *ffmpeg_get_frame(struct ffmpeg_video_t *video);
struct ffmpeg_video_t *ffmpeg_open_video(const char* filename, enum PixelFormat format);
void ffmpeg_close_and_free_video(struct ffmpeg_video_t *video);
char ffmpeg_video_finished(struct ffmpeg_video_t *video);
void ffmpeg_next_frame(struct ffmpeg_video_t *video);
int ffmpeg_first_video_stream(struct ffmpeg_video_t *video);
unsigned int ffmpeg_video_width(struct ffmpeg_video_t *video);
unsigned int ffmpeg_video_height(struct ffmpeg_video_t *video);
Imlib_Image *ffmpeg_get_frame_as_imlib(struct ffmpeg_video_t *video);
