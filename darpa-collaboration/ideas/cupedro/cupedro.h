
#pragma once

/**
 * Aaron Michaux, November 2011
 */

#ifdef __cplusplus
extern "C" {
#endif

  /** ----------------------------------------------------------------- C interface */
  #include <stdint.h>

  // This is a convenient method for passing an array of strings through
  // the scheme->c interface
  struct string_list_t {
    const char* value;
    struct string_list_t* next;
  };
  // Pass NULL when creating a new list
  struct string_list_t* prepend_to_string_list(struct string_list_t* list, const char* value);
  void free_string_list(struct string_list_t* list);

  /** Creates a new irobot-felzenszwalb context that runs on a single CUDA core
   * @param models: a singlely linked list of strings
   * @param pca_filename: csv file that contains pca data
   * @return: an untyped pointer to a cupedro object
   */
  void* cupedro_new(struct string_list_t* model_filenames, const char* pca_filename);
  void cupedro_delete(void* cupedro);

  // Only errors are printed when in quiet mode
  int cupedro_is_quiet(void* cupedro);
  void cupedro_set_quiet(void* cupedro, int quiet);

  // Get information on which models are loaded
  int cupedro_get_n_models(void* cupedro);
  const char* cupedro_get_model_filename(void* cupedro, int model_idx);
  const char* cupedro_get_model_name(void* cupedro, int model_idx);

  // Model theshold adjustments, where negative values imply overdetect
  double cupedro_get_thres_adj(void* cupedro, int model_idx);
  void cupedro_set_thres_adj(void* cupedro, int model_idx, double thresh_adjust);
  void cupedro_set_global_thres_adj(void* cupedro, double thresh_adjust);

  // The final cascade threshold
  double cupedro_get_final_cascade_thres(void* cupedro, int model_idx);
  void cupedro_set_final_cascade_thres(void* cupedro, int model_idx, double thresh_adjust);
  void cupedro_set_global_final_cascade_thres(void* cupedro, double thresh_adjust);

  // Setting maximum number of results
  int cupedro_get_max_results(void* cupedro);
  void cupedro_set_max_results(void* cupedro, int max_results);

  // Process a single image/frame
  void cupedro_detect(void* cupedro, uint32_t* argb_pixels, int width, int height);

  // Pick up any results from previously processed frame
  int cupedro_n_results(void* cupedro, int model_idx);
  int cupedro_result_size(void* cupedro, int model_idx, int result_index);
  double* cupedro_load_result(void* cupedro, int model_idx, int result_index);

  // If set, then boxes files are automatically written
  const char* cupedro_get_output_dir(void* cupedro);
  void cupedro_set_output_dir(void* cupedro, const char* output_dir);

  // Prints out cuda device information
  void print_cuda_device_information();

#ifdef __cplusplus
}
#endif
