/* LaHaShem HaAretz U'Mloah */

#ifndef HMM_DATA_H
#define HMM_DATA_H

#define MAX_DISCRETE   100	/* maximum number of discrete classes */
#define MIN_STD_DEV    0.01
#define MAX_KAPPA      100.0
#define SQRT_2PI       2.506628	/* calculated as sqrt(2.0*M_PI) */
#define LOG_2PI        1.837877	/* calculated as log(2.0*PI) */

struct ContFI {Real initialSigma;};

typedef Real (*LogLikeF)(Real, void *);
typedef int (*MaxF)(void *, Real **, RVec *, int, Real, Real *, int, int *);
typedef int FeatType;

typedef struct {Real mu; Real sigma;} Param;
typedef struct {Real mean; Real kappa;} VMParam;
typedef struct {int kk; Real p[MAX_DISCRETE];} DParam;
typedef struct {
  void **p;			/* model parameters */
  LogLikeF *lf;			/* likelihood functions */
  MaxF *mf;			/* maximisation functions */
  FeatType *ft;			/* feature types */
  void **fi;			/* additional feature info */
  int ii;			/* number of features */
} Ffm;

enum {FT_CONTINUOUS, FT_RADIAL, FT_DISCRETE, FT_OTHER}; /* Feature types */

/* utility functions */

Ffm *allocFFM(int ii);
void copyFFM(Ffm *dst_m, Ffm *src_m);
void displayFFM(FILE *f, Ffm *m);
void freeFFM(Ffm *m);

/* standard evaluation and maximisation functions */

Real kappaEstimate(Real r);
Real logGauss(Real x, void *param);
Real logVonMises(Real x, void *param);
Real logDiscrete(Real x, void *param);
int maxGauss(void *param, Real **x, RVec *weight, int ll, Real log_D,
	     Real *postpC, int c, int *c_ls);
int maxVonMises(void *param, Real **x, RVec *weight, int ll, Real log_D,
		Real *postpC, int c, int *c_ls);
float vonMisesOpt(float k);
int maxDiscrete(void *param, Real **x, RVec *weight, int ll, Real log_D,
		Real *postpC, int c, int *c_ls);

/* accessor functions */

FeatType getFeatType(Ffm *m, int i);
void *getFeatInfo(Ffm *m, int i);
void *getParam(Ffm *m, int i);
int getFeatNum(Ffm *m);
Real getInitialSigma(void);
void setInitialSigma(Real x);

/* setup functions */

void randomiseParams(Ffm *m);
void *setFeatType(Ffm *m, int i, FeatType ft, void *fi);
void setMaxFunc(Ffm *m, int i, MaxF f);
void setLogLikeFunc(Ffm *m, int i, LogLikeF f);
void *setParam(Ffm *m, int i, void *p);

/* model evaluation and maximisation functions */

void FFM_logL(Ffm *m, RVec logL, RMat data);
int FFM_maximise(Ffm *m, RMat *data, RVec *weight, int ll, Real log_D,
		  Real *postpC, int c, int *c_ls);

#endif

/* Tam V'Nishlam Shevah L'El Borei Olam */
