/* LaHaShem HaAretz U'Mloah */

#ifndef HMM_CONTROL_H
#define HMM_CONTROL_H

typedef struct {
  Hmm *hmm;			/* Hidden Markov Model */
  Ffm **ffm;			/* Data models for HMM states */
  int uu;			/* number of states in HMM */
} Model;

/* -- function definition -- */

/* -- utility functions -- */

void initGlobals(int ll, int uu, RMat *data);
Model *allocModel(int ii, int uu);
void copyModel(Model *dst_m, Model *src_m);
void randomiseModel(Model *m, int ut, int rab);
void defineContFeat(Model *m, int i, Real sigma);
void defineRadialFeat(Model *m, int i);
void defineDiscreteFeat(Model *m, int i);
void displayModel(FILE *f, Model *m);
void print_model(Model *m);
void freeModel(Model *m);

/* -- processing functions -- */

Real logLike(Model *m, RMat data);
int *best_state_sequence(Model *m, RMat data);
void force_init_globals(void);
void compute_posterior(Model **m, RMat *data, Real *prior, int *c_ls, int ll,
		       int cc, enum training_mode training_mode,
		       /* outputs */
		       Real *O, Real *like, Real **postpC);
int update(Model **m, Hmm **tmp_hmm, RMat *data, Real **postpC, Real log_D,
	   int *c_ls, int ll, int cc, enum training_mode training_mode,
	   int upper_triangular);

/* -- accessor functions -- */

void removeStates(Model *m, int *xu, Hmm* hmm);
int model_ii(Model *m);
int model_feature_type(Model *m, int i);
Real model_parameter(Model *m, int u, int i, int n);
int model_uu(Model *m);
Real model_a(Model *m, int u, int v);
Real model_b(Model *m, int u);
void set_model_parameter(Model *m, int u, int i, int n, Real x);
void set_model_a(Model *m, int u, int v, Real x);
void set_model_b(Model *m, int u, Real x);

#endif

/* Tam V'Nishlam Shevah L'El Borei Olam */
