
/**
 * Aaron Michaux, November 2011
 */

#include "cupedro.h"
#include <stdio.h>
#include <string.h>

#define CUPEDRO ((cupedro_rsrcs*) cupedro)

struct string_list_t* prepend_to_string_list(struct string_list_t* list, const char* value)
{
  struct string_list_t* new_node = new string_list_t();
  new_node->value = new char[strlen(value)+1];
  strcpy((char*) new_node->value, value);
  new_node->next = list;
  return new_node;
}

void free_string_list(struct string_list_t* list)
{
  while(list) {
    struct string_list_t* current = list;
    list = list->next;
    delete[] current->value;
    delete current;
  }
} 
 
#ifndef USE_IROBOT_FELZ

#warning "Building without the irobot Felzenswalb tracker"

/* Dummy interface for building without USE_IROBOT_FELZ  */
#define NOTIMPL (fprintf(stderr, "To implement this function, build with -DUSE_IROBOT_FELZ\n"))
void* cupedro_new(struct string_list_t* model_filenames, const char* pca_filename)
{ NOTIMPL; return NULL; }
void cupedro_delete(void* cupedro) { NOTIMPL; }

// Only errors are printed when in quiet mode
int cupedro_is_quiet(void* cupedro) { NOTIMPL; return 0; }
void cupedro_set_quiet(void* cupedro, int quiet) {NOTIMPL;}
  
// Get information on which models are loaded
int cupedro_get_n_models(void* cupedro) { NOTIMPL; return 0; }
const char* cupedro_get_model_filename(void* cupedro, int model_idx) { NOTIMPL; return ""; }
const char* cupedro_get_model_name(void* cupedro, int model_idx) { NOTIMPL; return ""; }

// Model theshold adjustments, where negative values imply overdetect
double cupedro_get_thres_adj(void* cupedro, int model_idx) { NOTIMPL; return 0.0f; }
void cupedro_set_thres_adj(void* cupedro, int model_idx, double thresh_adjust) { NOTIMPL; }
void cupedro_set_global_thres_adj(void* cupedro, double thresh_adjust) { NOTIMPL; }

// The final cascade threshold
double cupedro_get_final_cascade_thres(void* cupedro, int model_idx) { NOTIMPL; return 0.0f; }
void cupedro_set_final_cascade_thres(void* cupedro, int model_idx, double thresh_adjust) { NOTIMPL; }
void cupedro_set_global_final_cascade_thres(void* cupedro, double thresh_adjust) { NOTIMPL; }

// Process a single image/frame
void cupedro_detect(void* cupedro, uint32_t* argb_pixels, int width, int height) { NOTIMPL; }

// Pick up any results from previously processed frame
int cupedro_n_results(void* cupedro, int model_idx) { NOTIMPL; return 0; }
int cupedro_result_size(void* cupedro, int model_idx, int result_index) { NOTIMPL; return 0; }
double* cupedro_load_result(void* cupedro, int model_idx, int result_index) { NOTIMPL; return NULL; }

const char* cupedro_get_output_dir(void* cupedro) { NOTIMPL; return ""; }
void cupedro_set_output_dir(void* cupedro, const char* output_dir) { NOTIMPL; }

// Prints out cuda device information 
void print_cuda_device_information() { NOTIMPL; }

#ifdef BUILD_CMD_UTIL
int main(int, char**)
{
  NOTIMPL;
  return 0;
}
#endif

#else // USE_IROBOT_FELZ is defined

// ------------------------------------------------------------------- includes
#include <stdlib.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <sys/time.h>
#include <errno.h>
#include <Imlib2.h>

#include <vector>
#include <string>

#include "CudaFelzDetector.h"
#include "smart-array.hpp"

// ----------------------------------------------------------------- predefines
void write_detect_result(FILE* out, const felz_result& result, const char* name, std::vector<double>& back);
SmartArray<char> process_output_dir(const char* output_dir);
void transform_to_col_major_RBG(const DATA32* raw, float* dest,
					  int width, int height, int channels);
int cupedro_run_detector(const char* input_filename, const char* output_dir,
				 unsigned int n_models, const char** models, 
				 const char* pca_filename,
				 float threshold_adjustment,
				 float final_cascade_threshold,
				 int quiet);
void assert_bounds(void* cupedro, int model_idx);
SmartArray<char> strip_dir_and_ext(const char* filename, char ext_delim='.', char path_delim='/');
int find_extension_pos(const char* str, char ext_delim, char path_delim);

// A (rather crude) object that stores cuda-related resources
class cupedro_rsrcs
{
private:
  void operator=(const cupedro_rsrcs&);
  cupedro_rsrcs(const cupedro_rsrcs&);

public:
  CudaFelzDetector* cupedro; // irobot cuda detector
  float* image_col_major_device; // internally used by cupedro
  int width, height; // Size of the image on the GPU
  SmartArray<char> outdir; // The directory to write output files (none if size() == 0)
  SmartArray<char> buffer; // A temporary buffer for building file names
  SmartArray<float> im_buffer; // A temporary buffer for transforming images
  std::vector<std::string> model_filenames; // the list of model files in use
  std::vector<std::string> model_names; // striped directory and extension
  std::string pca_filename; // A reference to the pca-file in use
  FILE** fptrs; // An array of file pointers, some open/NULL
  int quiet; // only print error messages (if TRUE)

  // model_index => result_indx => array of doubles
  std::vector< std::vector< std::vector<double> > > results; // results from last detect

  cupedro_rsrcs(unsigned int n_models, const char** model_filenames, const char* pca_filename) 
    : cupedro(NULL), image_col_major_device(NULL), 
	width(0), height(0),
	outdir(), buffer(), im_buffer(),
	model_filenames(), model_names(), pca_filename(), fptrs(NULL), quiet(1), results()
  {
    // Setup the models
    this->model_filenames.reserve(n_models);
    for (int i = 0 ; i < n_models; i++) {
	this->model_filenames.push_back(model_filenames[i]);
	model_names.push_back(strip_dir_and_ext(model_filenames[i]).ptr());
    }
    this->pca_filename = pca_filename;
  
    // Setup the CudaFelzDetector object
    cupedro = new CudaFelzDetector(this->model_filenames, this->pca_filename.c_str());

    // We want the final global threshold at 0
    cupedro_set_global_final_cascade_thres(this, 0.0);
 
    // Potentially create a save file for every model
    fptrs = new FILE*[n_models];
    for(int i = 0; i < n_models; ++i) fptrs[i] = NULL;

    results.resize(n_models);
    for(int i = 0; i < n_models; ++i) results[i].resize(0);
  }

  ~cupedro_rsrcs()
  {
    // Close any open files
    if(fptrs) {
	for(int i = 0; i < this->model_filenames.size(); ++i) {
	  if(fptrs[i]) fclose(fptrs[i]);
	}
	delete[] fptrs;
    }

    if(image_col_major_device)
	CudaFelzDetector::deallocateDeviceImage(image_col_major_device);
  
    if(cupedro) delete cupedro;
  }
};

// -- A copy-on-write smart-pointer wrapper around image information
template<typename T> class Image
{
public:
  int width, height, channels;
  enum format_t { ARGB, RGB, COL_MAJOR_RGB, BGRA, NONE };
  format_t format;
  SmartArray<T> data; // Access the underlying memory with data.ptr()

  Image<T>() : width(0), height(0), channels(0), format(NONE), data() {}
};


#ifdef BUILD_CMD_UTIL
#include "ff-light.h"
void show_help();
Image<uint32_t> load_image(const char* filename);
void load_video_frame(video_handle video, SmartArray<uint8_t>& buffer, Image<uint32_t>& dest);
#endif

// ----------------------------------------------------------------- new_cupedro
void* cupedro_new(struct string_list_t* model_filenames, const char* pca_filename)
{
  // First unpack the singly-linked list
  std::vector<const char*> models;
  while(model_filenames) {
    models.push_back(model_filenames->value);
    model_filenames = model_filenames->next;
  }
  return (void*) new cupedro_rsrcs(models.size(), &models[0], pca_filename);
}

// ----------------------------------------------------------------- delete_cupedro
void cupedro_delete(void* cupedro)
{
  delete CUPEDRO;
}

// ----------------------------------------------------------------- assert bounds
void assert_bounds(void* cupedro, int model_idx)
{
  if(model_idx < 0 || model_idx >= CUPEDRO->model_filenames.size()) {
    fprintf(stderr, "Array index out of bounds, aborting\n");
    exit(1);
  }
}

// ----------------------------------------------------------------- cupedro quiet?
int cupedro_is_quiet(void* cupedro)
{
  return CUPEDRO->quiet;
}

void cupedro_set_quiet(void* cupedro, int quiet)
{
  CUPEDRO->quiet = quiet ? 1 : 0;
}

// ----------------------------------------------------------------- output directory
const char* cupedro_get_output_dir(void* cupedro)
{
  return CUPEDRO->outdir.ptr();
}

void cupedro_set_output_dir(void* cupedro, const char* output_dir)
{
  CUPEDRO->outdir = process_output_dir(output_dir);
  int buffer_size = 1024 + CUPEDRO->outdir.size();
  CUPEDRO->buffer.set_size(buffer_size); // building filenames
}

// ----------------------------------------------------------------- cupedro models
int cupedro_get_n_models(void* cupedro)
{
  return CUPEDRO->model_filenames.size();
}

const char* cupedro_get_model_filename(void* cupedro, int model_idx)
{
  return CUPEDRO->model_filenames[model_idx].c_str();
}

const char* cupedro_get_model_name(void* cupedro, int model_idx)
{
  return CUPEDRO->model_names[model_idx].c_str();
}

// ----------------------------------------------------------------- cupedro-threshold adj
double cupedro_get_thres_adj(void* cupedro, int model_idx)
{
  assert_bounds(cupedro, model_idx);
  return CUPEDRO->cupedro->getAdditiveThresholdAdjustment(model_idx);
}

void cupedro_set_thres_adj(void* cupedro, int model_idx, double thresh_adjust)
{
  cupedro_rsrcs& rsrc = *CUPEDRO;
  assert_bounds(cupedro, model_idx);
  rsrc.cupedro->setAdditiveThresholdAdjustment(model_idx, thresh_adjust);
  if(!rsrc.quiet) printf("Felz additive-threshold-adjustment (model[%d]) set to %f\n", model_idx, thresh_adjust);
}

void cupedro_set_global_thres_adj(void* cupedro, double thresh_adjust)
{
  cupedro_rsrcs& rsrc = *CUPEDRO;
  rsrc.cupedro->setAdditiveThresholdAdjustment(thresh_adjust);
  if(!rsrc.quiet) printf("Felz additive-threshold-adjustment set to %f\n", thresh_adjust);
}

// ----------------------------------------------------------------- cupedro-final cascade thres
double cupedro_get_final_cascade_thres(void* cupedro, int model_idx)
{
  assert_bounds(cupedro, model_idx);
  return CUPEDRO->cupedro->getFinalCascadeThreshold(model_idx);
}

void cupedro_set_final_cascade_thres(void* cupedro, int model_idx, double thresh_adjust)
{
  cupedro_rsrcs& rsrc = *CUPEDRO;
  assert_bounds(cupedro, model_idx);
  rsrc.cupedro->setFinalCascadeThreshold(model_idx, thresh_adjust);
  if(!rsrc.quiet) printf("Felz final-cascade-threshold (model[%d]) set to %f\n", model_idx, thresh_adjust);
}

void cupedro_set_global_final_cascade_thres(void* cupedro, double thresh_adjust)
{
  cupedro_rsrcs& rsrc = *CUPEDRO;
  rsrc.cupedro->setFinalCascadeThreshold(thresh_adjust);
  if(!rsrc.quiet) printf("Felz final-cascade-threshold set to %f\n", thresh_adjust);
}

// ----------------------------------------------------------------- max-results
int cupedro_get_max_results(void* cupedro)
{
  return CUPEDRO->cupedro->getMaxResults();
}

void cupedro_set_max_results(void* cupedro, int max_results)
{
  CUPEDRO->cupedro->setMaxResults(max_results);
}

// ----------------------------------------------------------------- seconds_difference
float seconds_difference(const timespec& end_ts, const timespec& start_ts)
{
  return (float) (end_ts.tv_sec - start_ts.tv_sec) 
    + ((float) (end_ts.tv_nsec - start_ts.tv_nsec)) * 0.000000001f;
}

// ----------------------------------------------------------------- detect
void cupedro_detect(void* cupedro, uint32_t* argb_pixels, int width, int height)
{
  const int n_channels = 3; // always 3 channels
  float transform_time = 0.0, copy_time = 0.0, detect_time = 0.0;
  timespec start_ts, end_ts;
  cupedro_rsrcs& rsrc = *CUPEDRO;

  // No effect if the size is already correct
  rsrc.im_buffer.set_size(width * height * n_channels);

  // We need the image in column-major-RGB-float format
  clock_gettime(CLOCK_REALTIME, &start_ts);
  transform_to_col_major_RBG(argb_pixels, rsrc.im_buffer.ptr(), width, height, n_channels);
  clock_gettime(CLOCK_REALTIME, &end_ts);
  transform_time = seconds_difference(end_ts, start_ts);

  // Deallocate image_col_major_device if it is the wrong size
  if(rsrc.image_col_major_device && (width != rsrc.width || height != rsrc.height)) {
    CudaFelzDetector::deallocateDeviceImage(rsrc.image_col_major_device);
    rsrc.image_col_major_device = NULL;
  }

  // Allocate space for image on GPU if necessary
  if(!rsrc.image_col_major_device) {
    rsrc.image_col_major_device = CudaFelzDetector::allocateDeviceImage(width, height, n_channels);
    rsrc.width = width;
    rsrc.height = height;
    // Pre-allocate detector data structures for given image size -- NOTE must this be done every time
    rsrc.cupedro->initDetector(rsrc.width, rsrc.height);
  }
  
  // Transfer image to GPU
  clock_gettime(CLOCK_REALTIME, &start_ts);
  CudaFelzDetector::copyImageToDevice(rsrc.im_buffer.ptr(), 
						  rsrc.image_col_major_device, width, height);
  clock_gettime(CLOCK_REALTIME, &end_ts);
  copy_time = seconds_difference(end_ts, start_ts);

  // Run detector
  clock_gettime(CLOCK_REALTIME, &start_ts);
  rsrc.cupedro->runDetector(width, height, rsrc.image_col_major_device);
  clock_gettime(CLOCK_REALTIME, &end_ts);
  detect_time = seconds_difference(end_ts, start_ts);

  // Gather results
  int num_results = rsrc.cupedro->getDetectionsCount();
  felz_result* results_host = rsrc.cupedro->getDetections();
  if(!rsrc.quiet) printf("%d detections total in %0.3fs (transform=%0.3fs, copy=%0.3fs, detect=%0.3fs)", num_results, (transform_time + copy_time + detect_time), transform_time, copy_time, detect_time);

  // Need to marshall the number of detects per file before writing them out...
  SmartArray<int> detection_counts(rsrc.model_filenames.size());
  detection_counts.memset(0);
  for(int i=0; i < num_results; ++i) {
    int model_idx = results_host[i].model_idx;
    detection_counts.ptr()[model_idx] += 1;
    const char* detect_name = basename(rsrc.model_filenames[model_idx].c_str());
    
    // Do we need to open a file?
    if(rsrc.outdir.size() > 0 && !rsrc.fptrs[model_idx]) {
	SmartArray<char>& buffer = rsrc.buffer;
	snprintf(buffer.ptr(), buffer.size(), "%s/voc4_%s.boxes", rsrc.outdir.ptr(), detect_name);
	rsrc.fptrs[model_idx] = fopen(buffer.ptr(), "w");
	if(!rsrc.fptrs[model_idx]) {
	  fprintf(stderr, 
		    "Failed to open file %s for writing -- some results will be missing.\n",
		    buffer.ptr());
	} else if(!rsrc.quiet) {
	  printf("Opening boxes file for writing: %s\n", buffer.ptr());
	}
    }
  }

  // Store the number of detections for each model type
  for(int i=0; i < rsrc.model_filenames.size(); ++i) {
    int count = detection_counts.ptr()[i];
    if(rsrc.fptrs[i]) {
	fprintf(rsrc.fptrs[i], "%d\n", count);
    }
    if(!rsrc.quiet) printf("%s%s=%d", (i==0 ? ": " : ", "), cupedro_get_model_name(cupedro, i), count);
    rsrc.results[i].clear();
    rsrc.results[i].reserve(count);
  }

  // Write/store results
  for(int i=0; i < num_results; ++i) {
    int model_idx = results_host[i].model_idx;
    rsrc.results[model_idx].push_back(std::vector<double>());
    // Write out to file
    write_detect_result(rsrc.fptrs[model_idx], results_host[i], 
				cupedro_get_model_name(cupedro, model_idx),
				rsrc.results[model_idx].back());
  }

  // End feedback line
  if(!rsrc.quiet) printf("\n");
}

// Pick up any results from previously processed frame
int cupedro_n_results(void* cupedro, int model_idx)
{
  int res = CUPEDRO->results[model_idx].size();
  return res;
}

int cupedro_result_size(void* cupedro, int model_idx, int result_index)
{
  int res = CUPEDRO->results[model_idx][result_index].size();
  return res;
}

double* cupedro_load_result(void* cupedro, int model_idx, int result_index)
{
  double* res = &(CUPEDRO->results[model_idx][result_index][0]);
  int n_res = cupedro_result_size(cupedro, model_idx, result_index);
  /*printf("bx = ["); // just some debugging
  for(int i = 0; i < n_res; ++i) {
    printf(" %0.4f", res[i]);
  }
  printf("]\n");*/
  return res;
}

/** ----------------------------------------------print_cuda_device_information
 * Just outputs the cuda device information to stdout
 */
void print_cuda_device_information()
{
  CudaFelzDetector::printCudaDeviceInformation();
}

// ----------------------------------------------------------------- load_image
/**
 * Loads the passed image, returns it in ARGB format. The image data is
 * copied once from an imlib buffer.
 */
#ifdef BUILD_CMD_UTIL
Image<uint32_t> load_image(const char* filename)
{
  Image<uint32_t> res;
  Imlib_Image image_handle = NULL;
  image_handle = imlib_load_image_immediately_without_cache(filename);
  if(image_handle) { 
    res.width = imlib_image_get_width();
    res.height = imlib_image_get_height();
    res.channels = 4; // ARGB
    res.data.set_size(res.width * res.height * res.channels);
    res.format = Image<uint32_t>::ARGB;

    DATA32* raw = imlib_image_get_data_for_reading_only(); // ARGB handle
    memcpy(res.data.ptr(), raw, res.width * res.height * sizeof(DATA32));
    
    imlib_free_image();
  }
  return res;
}
#endif

// ----------------------------------------------------------------- load_video_frame
/**
 * Loads the passed image, returns it in ARGB format. The image data is
 * copied once from an imlib buffer.
 */
#ifdef BUILD_CMD_UTIL
void load_video_frame(video_handle video, Image<uint32_t>& dest)
{
  dest.width = ff_video_width(video);
  dest.height = ff_video_height(video);
  dest.channels = 4; // 
  dest.format = Image<uint32_t>::ARGB;
  dest.data.set_size(dest.width * dest.height * dest.channels);

  // Get the frame data
  ff_get_frame(video, (uint8_t*) dest.data.ptr());
}
#endif


/** ----------------------------------------------- transform_to_col_major_RBG
 */
void transform_to_col_major_RBG(const DATA32* raw, float* dest,
					  int width, int height, int channels)
{
  unsigned char xych = 0;
  unsigned int h_by_w = height * width;

  for (int ch = 0; ch < channels; ch++) {
    for (int y = 0; y < height; y++) {
      for (int x = 0; x < width; x++) {
	  // 0: R ==> 0x00ff0000 >> 16
	  // 1: G ==> 0x0000ff00 >> 8
	  // 2: B ==> 0x000000ff >> 0
	  xych = (raw[y * width + x] & (0xff << ((2-ch)<< 3))) >> ((2-ch)<<3);
	  dest[h_by_w * ch + height * x + y] = xych;
      }
    }
  }
}

/** -------------------------------------------------------- process_output_dir
 * Removing trailing '/' if nessary. Also, replace empty string with "."
 * It is okay to pass NULL to this function.
 */
SmartArray<char> process_output_dir(const char* output_dir)
{
  // Work out the output directory -- make sure there is no trailing '/'
  SmartArray<char> outdir((output_dir ? (strlen(output_dir) + 2) : 0));
  if(output_dir) {
    char* ptr = outdir.ptr();
    char* end = outdir.ptr() + outdir.size();
    ptr += snprintf(ptr, ptr - end, "%s", output_dir);
    if(strcmp("", output_dir) == 0) ptr += snprintf(ptr, ptr - end, ".");
    if(ptr[-1] == '/') ptr[-1] = '\0';
  }
  return outdir;
}

/** -------------------------------------------------------- write_detect_result
 * Write the detection result to the output descriptor, and copy
 * the array of doubles into the specified vector of doubles.
 */
void write_detect_result(FILE* out, const felz_result& result, 
				 const char* name, std::vector<double>& res)
{
  res.clear();
  res.reserve(4 + result.nParts * 4 + 3);

  // The root filter
  res.push_back(result.min_x);
  res.push_back(result.min_y);
  res.push_back(result.max_x);
  res.push_back(result.max_y);

  // The part filters
  for(int i = 0; i < result.nParts; ++i) {
    res.push_back(result.best_part_locations[4*i + 0]);
    res.push_back(result.best_part_locations[4*i + 1]);
    res.push_back(result.best_part_locations[4*i + 2]);
    res.push_back(result.best_part_locations[4*i + 3]);
  }

  res.push_back(result.component_id);
  res.push_back(result.score);
  res.push_back(0); // used in scheme, more or less

  // output to file (if set)
  if(out) {
    for(int i = 0; i < res.size(); ++i) {
	fprintf(out, "%f ", res[i]); 
    }
    fprintf(out, "%s\n", name);
  }
}

/** -------------------------------------------------------------- find_extension_pos
 * Returns the index of the extension postion, accounting for corner cases
 */
int find_extension_pos(const char* str, char ext_delim, char path_delim) 
{
  // Degenerate case
  if (str == NULL) return 0;

  int len = strlen(str);
  int res = len; // Meaning that there is no extension

  const char* last_ext = strrchr (str, ext_delim);
  if(last_ext != NULL) {
    const char* last_path = (path_delim == 0) ? NULL : strrchr (str, path_delim);
    if(last_path != NULL) {
	if(last_path < last_ext) {
	  // There ext_delim appears /after/ path_delim
	  res = (int) (last_ext - str);
	}
	// The ext_delim appears /before/ path_delim, so do nothing
    } else {
	// There was no 'path' part 
	res = (int) (last_ext - str);
    }
  }

  // Corner cases
  if(res == 0) res = len; // .degnerate-case
  if(res > 0 && str[res-1] == path_delim) res = len; // /degenerate/.case

  return res;
}

/** -------------------------------------------------------------- strip_dir_and_ext
 * Strips the directory and extension from the passed filename
 */
SmartArray<char> strip_dir_and_ext(const char* filename, char ext_delim, char path_delim)
{
  const char* base = basename(filename);
  int ext_pos = find_extension_pos(base, ext_delim, path_delim);

  SmartArray<char> res(ext_pos + 1);
  res.ptr()[ext_pos] = 0; // null terminator
  memcpy(res.ptr(), base, ext_pos);

  return res;
}

/** -------------------------------------------------------------- run_detector
 *
 */
#ifdef BUILD_CMD_UTIL
int cupedro_run_detector(const char* input_filename, const char* output_dir,
			 unsigned int n_models, const char** models, 
			 const char* pca_filename,
			 float threshold_adjustment,
			 float final_cascade_threshold,
			 int quiet)
{
  bool res = false;
  video_handle video = NULL;
  SmartArray<uint8_t> video_frame_buffer(0);
  int video_frame_no = 0;
  void* rsrc = new cupedro_rsrcs(n_models, models, pca_filename);
  cupedro_set_quiet(rsrc, quiet);
  cupedro_set_output_dir(rsrc, output_dir);  

  if(!quiet) printf("Input filename = %s\n", input_filename);
  if(!quiet) printf("Selected Classes:\n");
  for (int i = 0 ; i < n_models; i++) {
    if(!quiet) printf("Class[%d] = %s\n", i, models[i]);
  }

  // First attempt to load input_file as if it were an image
  Image<uint32_t> image = load_image(input_filename);
  if(image.format == Image<uint32_t>::NONE) {
    // Attempt to set image as the first frame of a video
    video = ff_open_video(input_filename);
    if(video) {
	// Get the first frame
	load_video_frame(video, image);
    }
  }

  // Are we dead in the water?
  if(image.format == Image<uint32_t>::NONE) {
    fprintf(stderr, "Failed to load image/video file %s, aborting\n", input_filename);
    return 1;
  }

  // Set thresholds
  cupedro_set_global_thres_adj(rsrc, threshold_adjustment);
 
  if(!quiet) {
    printf("Running detector on %d model%s:\n", n_models, (n_models > 1 ? "s" : ""));
    for(int i = 0; i < n_models; ++i) {
	printf("    %s [thres=%f, cas=%f]\n", 
		 basename(cupedro_get_model_filename(rsrc, i)), 
		 cupedro_get_thres_adj(rsrc, i),
		 cupedro_get_final_cascade_thres(rsrc, i));
    }
  }

    // Process the image
  if(!quiet && video) printf("Processing frame %04d\n", video_frame_no);
  cupedro_detect(rsrc, image.data.ptr(), image.width, image.height);

  // Process remaning frames if we have a video
  if(video) {
    ff_next_frame(video);
    video_frame_no++;
    while(!ff_video_finished(video)) {
	if(!quiet && video) printf("Processing frame %04d\n", video_frame_no);
	load_video_frame(video, image);
	cupedro_detect(rsrc, image.data.ptr(), image.width, image.height);
	ff_next_frame(video);
	video_frame_no++;
    }
  }

  // Clean-up
  if(video) ff_close_and_free_video(video);
  cupedro_delete(rsrc);
  
  return res;
}
#endif

/** ----------------------------------------------------------------- show_help
 *
 */
#ifdef BUILD_CMD_UTIL
void show_help(const char* exec_name)
{
  const char* tabs = "   ";

  printf("\n");
  printf("%sUsage: %s [-m model-filename]+ -p pca-filename [-t threshold-adj] [-o output-directory] FILE\n", tabs, exec_name);
  printf("%s\n", tabs);
  printf("%sRuns Felzenswalb on FILE (either a movie or single image). Multiple models\n", tabs);
  printf("%scan be specified, in which case, mulitple boxes files are written. The resulting\n", tabs);
  printf("%sboxes files are written to the specified output-directory. (Default is ./)\n", tabs);
  printf("%s\n", tabs);
  printf("%sThreshold-adjusment is an optional floating point argument, where negative values\n", tabs);
  printf("%scause overdetection.\n", tabs);
  printf("%s\n", tabs);
  printf("%sSee the irobot documentation for creating the pca-filename, and generating compatible\n", tabs);
  printf("%smodel files.\n", tabs);
  printf("%s\n", tabs);
  printf("%sExample: \n", tabs);
  printf("%s\n", tabs);
  printf("%s%sLD_LIBRARY_PATH=/usr/local/cuda/lib64 ./%s -m models/model_2006_car -m models/model_2007_person -p models/felz_pca_coeff.csv test.mov\n", tabs, tabs, exec_name);
  printf("\n");
}
#endif

/** ---------------------------------------------------------------------- main
 *
 */
#ifdef BUILD_CMD_UTIL
int main(int argc, char** argv)
{
  // -- Parse cmd-line arguments
  if(argc == 0) {
    fprintf(stderr, "Type --help for help\n");
    exit(1);
  }
  
  std::vector<const char*> model_files;
  const char* input_file = NULL;
  const char* output_dir = NULL;
  const char* pca_file = NULL;
  float threshold = 0.0f;

  bool do_show_help = false;
  int i = 1; // Index into argv
  while(i < argc) {
    if(strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
	do_show_help = true;
    } else if(strcmp(argv[i], "-m") == 0) {
	// A model is to follow
	if(++i == argc) {
	  fprintf(stderr, "Expected filename after '-m' switch, aborting.\n");
	  exit(1);
	}
	model_files.push_back(argv[i]);
    } else if(strcmp(argv[i], "-t") == 0) { 
	if(++i == argc) {
	  fprintf(stderr, "Expected threshold after '-t' switch, aborting.\n");
	  exit(1);
	}
	threshold = atof(argv[i]);
    } else if(strcmp(argv[i], "-p") == 0) {
	if(++i == argc) {
	  fprintf(stderr, "Expected filename after '-p' switch, aborting.\n");
	  exit(1);
	}
	pca_file = argv[i];
    } else if(strcmp(argv[i], "-o") == 0) {
	if(++i == argc) {
	  fprintf(stderr, "Expected directory after '-o' switch, aborting.\n");
	  exit(1);
	}
	output_dir = argv[i];
    } else {
	if(input_file != NULL) {
	  fprintf(stderr, "Expected one, and only one, input file, aborting.\n");
	  exit(1);
	}
	input_file = argv[i];
    }

    i++;
  }

  if(do_show_help) {
    show_help(basename(argv[0]));
    exit(0);
  }

  // Check the sanity of inputs
  if(input_file == NULL) {
    fprintf(stderr, "No input files, aborting.\n"); 
    exit(1);
  }
  
  // Default to the pwd
  if(output_dir == NULL) {
    output_dir = "";
  }

  if(pca_file == NULL) {
    fprintf(stderr, "PCA file was not specified, aborting.\n"); 
    exit(1);
  }

  if(model_files.size() == 0) {
    fprintf(stderr, "No model files, aborting.\n");
    exit(1);
  }

  // Ready for action
  int res =  cupedro_run_detector(input_file, output_dir, model_files.size(), &model_files[0], 
				    pca_file, threshold, 0.0f, 0);
  return res;
}
#endif


#endif // use irobot felz


