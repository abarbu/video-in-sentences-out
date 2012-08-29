#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <sys/mman.h>
#include <zlib.h>
#include <errno.h>

#include <iu/iucore.h>
#include <iu/iuio.h>
#include <iu/iumath.h>
#include <fl/flowlib.h>
#include <cuda_runtime.h>

// FIXME Andrei char *bgra suffers from endianess issues
extern "C" float *bgra_to_float_greyscale(char *bgra, int width, int height) {
  float *out = (float*)malloc(sizeof(float)*width*height);
  for (int y = 0; y < height; ++y) 
    for (int x = 0; x < width; ++x)
      out[x+y*width] = (0.114 * (float)bgra[4*(x+y*width)] 
			+ 0.587 * (float)bgra[4*(x+y*width)+1] 
			+ 0.299 * (float)bgra[4*(x+y*width)+2]) / 255;
  return out;
}

/* TODO Andrei This API is bad because it does twice the number of copies */
/* takes greyscale images of floats */
extern "C" double *flowlib_optical_flow(float *image1, float *image2, 
			     int width, int height) {
  fl::FlowLib flow(0);

  // read images using imageutilities
  // iu::ImageGpu_32f_C1 *cu_im1 = iu::imread_cu32f_C1("/tmp/1.ppm");
  // iu::ImageGpu_32f_C1 *cu_im2 = iu::imread_cu32f_C1("/tmp/2.ppm");
  // if(!flow.setInputImages(cu_im1, cu_im2)) {
  //   fprintf(stderr,"flowlib isn't ready");
  //   abort();
  // }

  iu::ImageCpu_32f_C1 im1(image1, width, height, width*sizeof(float));
  iu::ImageCpu_32f_C1 im2(image2, width, height, width*sizeof(float));
  iu::ImageGpu_32f_C1 cu_im1(IuSize(width, height));
  iu::ImageGpu_32f_C1 cu_im2(IuSize(width, height));
  iu::copy(&im1, &cu_im1);
  iu::copy(&im2, &cu_im2);
  
  if(!flow.setInputImages(&cu_im1, &cu_im2)) {
    fprintf(stderr,"flowlib isn't ready");
    abort();
  }

  // if(!flow.setInputImages_32f_C1R(image1, image2, 
  // 				  width*sizeof(float), 
  // 				  IuSize(width, height))) {
  //   fprintf(stderr,"flowlib isn't ready");
  //   abort();
  // }

  flow.parameters().model = fl::HL1_ILLUMINATION_PRIMAL_DUAL;
  flow.parameters().iters = 100;
  flow.parameters().warps = 10;
  flow.parameters().scale_factor = 0.8f;
  flow.parameters().lambda = 40.0f;
  flow.parameters().gamma_c = 0.01f;

  flow.calculate();

  IuSize result_size;
  flow.getSize(flow.parameters().stop_level, result_size);
  
  iu::ImageGpu_32f_C1 cu_u(result_size);
  iu::ImageGpu_32f_C1 cu_v(result_size);
  iu::ImageCpu_32f_C1 u(result_size);
  iu::ImageCpu_32f_C1 v(result_size);

  flow.getU_32f_C1(flow.parameters().stop_level, &cu_u);
  flow.getV_32f_C1(flow.parameters().stop_level, &cu_v);
  iu::copy(&cu_u, &u);
  iu::copy(&cu_v, &v);

  float *u_data = u.data(), *v_data = v.data();

  double *result = (double*)malloc(sizeof(double)*width*height*2);
  
  for(int x = 0; x < width; ++x)
    for(int y = 0; y < height; ++y) {
      result[x+y*width] = (double) u_data[x+y*width];
      result[x+y*width+width*height] = (double) v_data[x+y*width];
    }
  return result;
}
