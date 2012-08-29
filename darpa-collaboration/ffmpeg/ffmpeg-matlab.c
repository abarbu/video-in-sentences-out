#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>

#include <stdio.h>
#include <stdlib.h>
#include <Imlib2.h>
#include <mex.h>
#include <Imlib2.h>

// ffmpegOpenVideo('test.mov'); imshow(ffmpegGetFrame()); ffmpegNextFrame(); imshow(ffmpegGetFrame()); ffmpegCloseVideo();

static AVFormatContext *pFormatCtx = NULL;
static int videoStream = -1;
static AVCodecContext *pCodecCtx = NULL;
static AVFrame *pFrame = NULL;
static AVFrame *pFrameRGB24 = NULL;
static uint8_t *buffer = NULL;
static struct SwsContext *img_convert_ctx = NULL;
static AVPacket packet;
static int frame = -1;
static int videoFinished = 1;

int firstVideoStream(AVFormatContext *pFormatCtx) {
  if(av_find_stream_info(pFormatCtx)<0) {
    mexErrMsgTxt("error: Can't get video stream information");
  }
  for(int i=0; i < pFormatCtx->nb_streams; i++)
    if(pFormatCtx->streams[i]->codec->codec_type==CODEC_TYPE_VIDEO)
      return i;
  mexErrMsgTxt("error: Can't find first video stream");
  exit(-1);
}

AVFormatContext *openVideo(char* filename) {
  AVFormatContext *pFormatCtx;
  av_register_all();
  if(av_open_input_file(&pFormatCtx, filename, NULL, 0, NULL)!=0)
    mexErrMsgIdAndTxt("ffmpeg:open", "error: Can't open video '%s'", filename);
  return pFormatCtx;
}

AVCodecContext *getCodec(AVFormatContext *pFormatCtx, int videoStream) {
  AVCodecContext *pCodecCtx = pFormatCtx->streams[videoStream]->codec;
  AVCodec *pCodec = avcodec_find_decoder(pCodecCtx->codec_id);
  if(pCodec==NULL)
    mexErrMsgTxt("error: Unsupported codec!");
  if(avcodec_open(pCodecCtx, pCodec)<0)
    mexErrMsgTxt("error: Can't open codec!");
  return pCodecCtx;
}

void matlabOpenVideo(char *filename) {
  pFormatCtx = openVideo(filename);
  videoStream = firstVideoStream(pFormatCtx);
  pCodecCtx = getCodec(pFormatCtx,videoStream);
  pFrame = avcodec_alloc_frame();
  pFrameRGB24 = avcodec_alloc_frame();
  if(!pFrameRGB24 || !pFrame) {
    mexErrMsgTxt("error: Can't allocate frame!");
  }
  buffer = (uint8_t *)av_malloc(avpicture_get_size(PIX_FMT_RGB24,
						   pCodecCtx->width,
						   pCodecCtx->height) *
				sizeof(uint8_t));
  avpicture_fill((AVPicture *)pFrameRGB24, buffer, PIX_FMT_RGB24,
		 pCodecCtx->width, pCodecCtx->height);
  img_convert_ctx = sws_getContext(pCodecCtx->width, pCodecCtx->height,
				   pCodecCtx->pix_fmt,
				   pCodecCtx->width, pCodecCtx->height,
				   PIX_FMT_RGB24, SWS_BICUBIC,
				   NULL, NULL, NULL);
  videoFinished = 0;
  frame = 0;
  av_init_packet(&packet);
  matlabNextFrame();
}

void matlabNextFrame() {
  if(videoFinished == 2) {
    videoFinished = 1;
  } else {
    av_free_packet(&packet);
    int frameFinished;
    int nextFrameValid = av_read_frame(pFormatCtx, &packet) >= 0;
    if(nextFrameValid && packet.stream_index==videoStream) {
      avcodec_decode_video(pCodecCtx, pFrame, &frameFinished,
			   packet.data, packet.size);
      if(frameFinished) frame++;
      else matlabNextFrame();
    } else if(nextFrameValid) {
      matlabNextFrame();
    } else if(!videoFinished && !nextFrameValid) {
      // This is required because ffmpeg hangs on to one frame internally
      // so we need to read one past the end of the video
      videoFinished = 2;
      avcodec_decode_video(pCodecCtx, pFrame, &frameFinished, 0, 0);
      frame++;
    }
  }
}

void matlabClose() {
  av_free(buffer);
  buffer = NULL;
  av_free(pFrameRGB24);
  pFrameRGB24 = NULL;
  av_free(pFrame);
  pFrame = NULL;
  avcodec_close(pCodecCtx);
  pCodecCtx = NULL;
  av_close_input_file(pFormatCtx);
  pFormatCtx = NULL;
  videoFinished = 1;
}

void
mexFunction (int nlhs, mxArray *plhs[], int nrhs,
	       const mxArray *prhs[])
{
  if(nrhs < 1) mexErrMsgTxt("Must have at least 1 input argument");

  int buflen = mxGetN(prhs[0])+1;
  char *functionName = mxCalloc(buflen, sizeof(char));
  mxGetString(prhs[0], functionName, buflen);

  if(!strncmp(functionName, "open", buflen)) {
    if(nrhs != 2) mexErrMsgTxt("Open needs one argument, a filename");
    if(nlhs != 0) mexErrMsgTxt("Open doesn't return anything");
    if(pFormatCtx) mexErrMsgTxt("A video is already open");
    int buflen = mxGetN(prhs[1])+1;
    char *filename = mxCalloc(buflen, sizeof(char));
    mxGetString(prhs[1], filename, buflen);
    matlabOpenVideo(filename);
    mxFree(filename);
  } else if(!strncmp(functionName, "next", buflen)) {
    if(nrhs != 1) mexErrMsgTxt("Next takes no arguments");
    if(nlhs > 1) mexErrMsgTxt("Next returns at most one value");
    if(videoFinished == 1) mexErrMsgTxt("Can't go to next frame, video stream is finished");
    if(!pFormatCtx) mexErrMsgTxt("There is no open video");
    matlabNextFrame();
    plhs[0] = mxCreateDoubleMatrix(1, 1, mxREAL);
    double* ptr = mxGetData(plhs[0]);
    ptr[0] = frame;
  } else if(!strncmp(functionName, "get", buflen)) {
    if(nrhs != 1) mexErrMsgTxt("Get takes no arguments");
    if(nlhs != 1) mexErrMsgTxt("Get returns one value");
    if(!pFormatCtx) mexErrMsgTxt("There is no open video");
    printf("Get frame %d\n", frame);
    mwSize dims[3] = { pCodecCtx->width, pCodecCtx->height, 3 };
    mxArray *data = mxCreateNumericArray(3, dims, mxUINT8_CLASS, mxREAL);
    sws_scale(img_convert_ctx, (const uint8_t * const*)pFrame->data,
	      pFrame->linesize, 0,
	      pCodecCtx->height,
	      pFrameRGB24->data, pFrameRGB24->linesize);
    for(int h = 0; h < pCodecCtx->height; ++h)
      for(int w = 0; w < pCodecCtx->width; ++w) {
	((unsigned char *)mxGetData(data))[h*pCodecCtx->width + w] =
	  buffer[(h*pCodecCtx->width + w)*3];
	((unsigned char *)mxGetData(data))[h*pCodecCtx->width + w + pCodecCtx->height * pCodecCtx->width] =
	  buffer[((h*pCodecCtx->width + w) * 3) + 1];
	((unsigned char *)mxGetData(data))[h*pCodecCtx->width + w + (pCodecCtx->height * pCodecCtx->width * 2)] =
	  buffer[((h*pCodecCtx->width + w) * 3) + 2];
      }
    plhs[0] = data;
  } else if(!strncmp(functionName, "finished", buflen)) {
    if(!pFormatCtx) mexErrMsgTxt("There is no open video");
    plhs[0] = mxCreateNumericMatrix(1, 1, mxUINT8_CLASS, mxREAL);
    ((unsigned char *)mxGetData(plhs[0]))[0] = (char) (videoFinished == 1);
  } else if(!strncmp(functionName, "close", buflen)) {
    if(nrhs != 1) mexErrMsgTxt("Close takes no arguments");
    if(nlhs != 0) mexErrMsgTxt("Close doesn't return anything");
    if(!pFormatCtx) mexErrMsgTxt("There is no open video");
    matlabClose();
  } else {
    mexErrMsgTxt("Not a valid matlab-ffmpeg command");
  }

  return;
}
