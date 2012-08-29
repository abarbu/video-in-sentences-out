#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswscale/swscale.h>
#include <Imlib2.h>

struct point_t { uint16_t x; uint16_t y; };

enum superpixel_id_type_t {
  ID_Number = 0,
  ID_Vector,
  ID_List,
  ID_Boolean
};

struct superpixel_id_t {
  int type;
  int number;
};

struct superpixel_t {
  struct superpixel_id_t name;
  uint16_t next_length;
  struct superpixel_id_t* next;
  struct superpixel_id_t parent;
  uint16_t children_length;
  struct superpixel_id_t* children;
  float vx, vy;
  uint16_t pixels_length;
  struct point_t* pixels;
};

struct superpixels_t {
  int width, height, number;
  struct superpixel_t* superpixel;
};

struct superpixel_id_t read_superpixel_id(FILE* file);

struct superpixels_t* read_superpixels(char *filename, int w, int h);

int* superpixel_map(struct superpixels_t* superpixels);
char same_superpixel_name_int(struct superpixel_id_t id, int name);
int safe_ref(int *map, int x, int y, int w, int h);
int is_outline_superpixel_pixel(int *map, struct superpixel_id_t id, int x, int y, int w, int h);
void color_superpixel(Imlib_Image *image, struct superpixels_t* superpixels,
		      int superpixel, int r, int g, int b, int fill);
int superpixel_center_x(struct superpixel_t* s);
int superpixel_center_y(struct superpixel_t* s);
float vmagnitude(float x, float y);
uint32_t* read_binary_superpixel_map(char *filename);
int superpixel_map_ref(uint32_t* map, int x, int y, int w);

double *read_flo_from_stream(FILE *file);
int32_t* read_flo_size_from_stream(FILE *file);
void write_flo_to_stream(double *matrix, 
			 const unsigned width, const unsigned height,
			 FILE *stream);


double* read_optical_flow_ssv(char *filename, unsigned height, unsigned width);
double *read_optical_flow_ssv_gz(char *filename,
				 unsigned height,
				 unsigned width);
double *average_optical_flow_ssv_from_c(double *ssv,
					unsigned height,
					unsigned width,
					unsigned xl,
					unsigned xh,
					unsigned yl,
					unsigned yh);
double *integral_optical_flow(double *flow,
			      const unsigned height,
			      const unsigned width);
double integral_optical_flow_area(double *integral_flow,
				  const unsigned height,
				  const unsigned width,
				  unsigned x1, unsigned y1,
				  unsigned x2, unsigned y2);

double *euclidean_1d_dt(double *f, unsigned n);

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

uint8_t *ffmpeg_get_frame(struct ffmpeg_video_t *video);
struct ffmpeg_video_t *ffmpeg_open_video(char* filename);
void ffmpeg_close_and_free_video(struct ffmpeg_video_t *video);
char ffmpeg_video_finished(struct ffmpeg_video_t *video);
void ffmpeg_next_frame(struct ffmpeg_video_t *video);
int ffmpeg_first_video_stream(struct ffmpeg_video_t *video);
unsigned int ffmpeg_video_width(struct ffmpeg_video_t *video);
unsigned int ffmpeg_video_height(struct ffmpeg_video_t *video);
double ffmpeg_video_frame_rate(struct ffmpeg_video_t *video);
Imlib_Image *ffmpeg_get_frame_as_imlib(struct ffmpeg_video_t *video);

double *read_mat_double_variable(const char *file, const char *name);
double *read_mat_double_from_struct(const char *file,
				    const char *model,
				    const char *name);

const int* imlib_get_text_dimention (const char *text);

struct box_line_t {
  double *values;
  int nr_values;
  char *name;
  struct box_line_t *next;
};

struct box_line_t *read_boxes_file(const char *filename);
struct box_line_t *read_boxes_from_buffer(char *buffer);

typedef double real;

struct dim3array
{
  int rows;
  int cols;
  int channels;
  real* data;
};

/** You must call free(...) on the result.data */
struct dim3array* pedro_resize_image(const real* im, int height, int width, int channels, real scale);

/** You must call free(...) on the result.data */
struct dim3array* pedro_features(const real* im, int height, int width, int channels, int sbin);

TSCP bool_tscp(int bool);
int tscp_bool(TSCP bool);

float *bgra_to_float_greyscale(int *bgra, int width, int height);
double *flowlib_optical_flow(float *image1, float *image2, 
			     int width, int height);

void setCudaDevice(int device);

