/* LaHaShem HaAretz U'Mloah */

#ifndef HMM_H
#define HMM_H

/* Macros */

#define NEGATIVE_INFINITY (-1.0/0.0)
#define LOG_MATH_PRECISION 35.0
#ifndef PI
#define PI 3.14159265358979323846
#endif

#define VLENGTH(v) ((v)->x)
#define ROWS(m) ((m)->y)
#define COLUMNS(m) ((m)->x)
#define VECTOR(x) ((x)->v)
#define MATRIX(x) ((x)->m)
#define HMM_STATES(x) ((x)->uu)

#ifdef _______FLOAT_REAL
typedef float Real;
#else
typedef double Real;
#endif

Real SQR(Real arg);

#define MACH_EPS 1e-7
#define IS_ZERO(x) (-MACH_EPS<(x)&&(x)<MACH_EPS)

#ifndef MIN
#define MIN(a,b) ((a)>(b)?(b):(a))
#endif
#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#endif

#ifndef TRUE
#define TRUE (0==0)
#endif

#ifndef FALSE
#define FALSE (0!=0)
#endif

#define safe_malloc(x) _safe_malloc((x), __LINE__, __FILE__)

#define DISPLAY_HMM_MAX_U 10

#define MAXIT 60
#define UNUSED (-1.11e30)
#define SIGN(a,b) ((b)>0.0?fabs(a):-fabs(a))

struct RVecStruct {size_t x; Real *v;};
struct RMatStruct {size_t x; size_t y; Real **m;};
struct RMat3dStruct {size_t x; size_t y; size_t z; Real ***m;};

typedef struct RVecStruct *RVec;
typedef struct RMatStruct *RMat;
typedef struct RMat3dStruct *RMat3d;

typedef struct {
  RMat logA;
  RVec logB;
  int uu;
} Hmm;

enum training_mode {HMM_ML, HMM_DT};

/* -- utility functions -- */

Hmm *allocHMM(int uu);
void constantHMM(Hmm *m, int upper_triangular);
void copyHMM(Hmm *m1, Hmm *m2);
void displayHMM(FILE *f, Hmm *m);
void freeHMM(Hmm *m);
void normaliseHMM(Hmm *m, int upper_triangular);
int normaliseHMMlinear(Hmm *m, int upper_triangular, enum training_mode train_mode, int* xu);
void randomiseHMM(Hmm *m, int upper_triangular);
void zeroHMM(Hmm *m);
void zeroHMMlinear(Hmm *m);

/* -- processing functions -- */

void HMM_calcAlphas(Hmm *m, RVec *logL);
void HMM_calcDeltas(Hmm *m, RVec *logL);
void HMM_initGlobals(int uu, int tt);
Real HMM_logL(Hmm *m, RVec *logL);
int *HMM_best_state_sequence(Hmm *m, RVec *logL);
Real HMM_updateModel(Hmm *m, Hmm *new, RVec *logL, RVec *gamma, Real log_D,
		     Real postpC, int c, int c_ls,
		     enum training_mode training_mode);
Real my_exp(Real x);
Real my_log(Real x);
Real my_atan2(Real x, Real y);
RMat *allocate_rmat_vector(size_t n);
RVec allocRVec(size_t x);
RMat allocRMat(size_t y, size_t x);
RMat3d allocRMat3d(size_t z, size_t y, size_t x);
void free_rmat_vector(RMat *v);
void freeRVec(RVec v);
void freeRMat(RMat m);
void freeRMat3d(RMat3d m);
Real *addRVec(Real *u, const Real *v, int size);
Real *copyRVec(Real *u, const Real *v, int size);
Real dotProdRVec(const Real *u, const Real *v, int size);
Real normaliseRVec(Real *v, int size);
Real rmat_get(RMat rmat, int i, int j);
void rmat_set(RMat rmat, int i, int j, Real x);
void rmat_vector_set(RMat *v, int i, RMat r);
Real *scaleRVec(Real *v, Real k, int size);
Real sumOfLogs(Real *v, int size);
Real sumRVec(Real *v, int size);
Real add_exp(Real e1, Real e2);
float lbessi0(float x);
float lbessi1(float x);
float zriddr(float (*func)(float), float x1, float x2, float xacc);
void free_c_vector(void *v);
int c_int_vector_ref(int *v, int x);
void *_safe_malloc(size_t size, int line, char *file);
void panic(const char *error_text, ...);

#endif

/* Tam V'Nishlam Shevah L'El Borei Olam */
