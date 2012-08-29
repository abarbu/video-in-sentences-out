// g++ background_gmm.cpp -o background_gmm `pkg-config opencv --libs --cflags`
// ./background_gmm Fall7_A1_C2_Act1_2_DOWNTOWN1A3_FC_MIDD_47a00c19-c5af-11df-99ed-e80688cb869a.avi Approach1_A1_C1_Act1_4_DOWNTOWN1A3_MC_MIDD_4798658a-c5af-11df-9b88-e80688cb869a.avi Attach4_A1_C1_Act2_DOWNTOWN1A3_MC_MIDD_479d2130-c5af-11df-b946-e80688cb869a.avi

#include <opencv2/video/background_segm.hpp>
#include <opencv2/highgui/highgui.hpp>
#include <stdio.h>
#include <string>
#include <vector>

using namespace std;

CvGaussBGStatModelParams defaultParametersGMM() {
  CvGaussBGStatModelParams parameters;
  parameters.bg_threshold = 0.7;
  parameters.std_threshold = 2.5;
  parameters.win_size = 200;
  parameters.n_gauss = 5;
  parameters.weight_init = 0.05;
  parameters.variance_init = 30;
  //  parameters.minArea = 15.f;
  parameters.minArea = 30.f;
  return parameters;
}

CvFGDStatModelParams defaultParametersFGD() {
  CvFGDStatModelParams parameters;
  parameters.Lc = 128;
  parameters.N1c = 15;
  parameters.N2c = 25;
  parameters.Lcc = 64;
  parameters.N1cc = 25;
  parameters.N2cc = 40;
  parameters.is_obj_without_holes = 1;
  parameters.perform_morphing = 1;
  parameters.alpha1 = 1;
  parameters.alpha2 = 0.005f;
  parameters.alpha3 = 0.1f;
  parameters.delta = 2;
  parameters.T = 0.9f;
  parameters.minArea = 0.15f;
  return parameters;
}

enum ModelType {
  GMM_MODEL,
  FGD_MODEL
};

struct modelParameters {
  ModelType type;
  CvGaussBGStatModelParams gmm;
  CvFGDStatModelParams fgd;
};

CvBGStatModel* createBGModel(IplImage *frame, modelParameters& parameters) {
  switch(parameters.type) {
  case GMM_MODEL: return cvCreateGaussianBGModel(frame, &parameters.gmm);
  case FGD_MODEL: return cvCreateFGDStatModel(frame, &parameters.fgd);
  default: printf("Bad model type"); exit(1);
  }
}

CvBGStatModel* train(string video, CvBGStatModel* model,
		     modelParameters& parameters, int learning_rate) {
  CvCapture *cap = cvCaptureFromFile(video.c_str());
  IplImage *tmp_frame = NULL;
  if(!model) model = createBGModel( cvQueryFrame(cap), parameters );
  while((tmp_frame = cvQueryFrame(cap)))
    cvUpdateBGStatModel( tmp_frame, model, learning_rate );
  cvReleaseCapture(&cap);
  return model;
}

int main(int argc, char** argv)
{
  if( argc < 2 ) {
    printf("usage: ./background_gmm video_in [training*]");
    printf("  if no training videos are given model will update for the input video");
  }

  char* test_video = argv[1];
  CvCapture *cap = cvCaptureFromFile(test_video);
  if( !cap ) {
    printf("can't open test video file\n");
    exit(1);
  }

  vector<string> videos;
  for(int i = 2; i < argc; ++i) videos.push_back(argv[i]);

  modelParameters parameters;
  parameters.type = FGD_MODEL;
  parameters.gmm = defaultParametersGMM();
  parameters.fgd = defaultParametersFGD();

  CvBGStatModel* model = 0;

  for(vector<string>::iterator iter = videos.begin(); iter != videos.end(); ++iter)
    model = train(*iter, model, parameters, -1);

  bool update_model = !model;

  cvNamedWindow("BG", 1);
  cvNamedWindow("FG", 1);

  IplImage *tmp_frame = cvQueryFrame(cap);

  for( int fr = 1;tmp_frame; tmp_frame = cvQueryFrame(cap), fr++ )
  {
    if(!model) {

      model = createBGModel( tmp_frame, parameters );
      printf("cvTypeOf %p\n", cvTypeOf(model));
      continue;
    }

    double t = (double)cvGetTickCount();
    cvUpdateBGStatModel( tmp_frame, model, update_model ? -1 : 0);
    t = (double)cvGetTickCount() - t;
    printf( "%d. %.1f\n", fr, t/(cvGetTickFrequency()*1000.) );
    cvShowImage("BG", model->background);
    cvShowImage("FG", model->foreground);
    char k = cvWaitKey(5);
    if( k == 27 ) break;
    if( k == ' ' )
    {
      update_model = !update_model;
      if(update_model)
	printf("Background update is on\n");
      else
	printf("Background update is off\n");
    }

    char str[100];
    snprintf(str, 100, "foreground-%04d.pgm", fr);
    cvSaveImage(str, model->foreground);
  }

  cvReleaseBGStatModel( &model );
  cvReleaseCapture(&cap);

  return 0;
}
