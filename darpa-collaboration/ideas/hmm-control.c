/* LaHaShem HaAretz U'Mloah */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "hmm.h"
#include "hmm-rand.h"
#include "hmm-data.h"
#include "hmm-control.h"

static RVec **g_logL = NULL;
static RVec **g_gamma = NULL;
static RVec **g_gamma_r = NULL;
static int g_ll;
static int g_uu;
static RMat *g_lastData = NULL;

/* -- function declarations -- */

/* -- utility functions -- */

/* initGlobals - Allocates or reallocates global variables for the modules.
 *  Could be improved to reallocate space only when it is necessary.
 */
void initGlobals(int ll, int uu, RMat *data)
{ int l, u;
  if (g_logL!=NULL)
  { for (l = 0; l<g_ll; l++)
    { for (u = 0; u<g_uu; u++)
      { freeRVec(g_logL[l][u]);
	freeRVec(g_gamma[l][u]);}
      free(g_logL[l]);
      free(g_gamma[l]);}
    for (u = 0; u<g_uu; u++) free(g_gamma_r[u]);
    free(g_logL);
    free(g_gamma);
    free(g_gamma_r);}
  g_logL = (RVec **)safe_malloc(sizeof(RVec *)*ll);
  g_gamma = (RVec **)safe_malloc(sizeof(RVec *)*ll);
  g_gamma_r = (RVec **)safe_malloc(sizeof(RVec *)*uu);
  for (l = 0; l<ll; l++)
  { g_logL[l] = (RVec *)safe_malloc(sizeof(RVec)*uu);
    g_gamma[l] = (RVec *)safe_malloc(sizeof(RVec)*uu);
    for (u = 0; u<uu; u++)
    { g_logL[l][u] = allocRVec(COLUMNS(data[l]));
      g_gamma[l][u] = allocRVec(COLUMNS(data[l]));}}
  for (u = 0; u<uu; u++)
  { g_gamma_r[u] = (RVec *)safe_malloc(sizeof(RVec)*ll);
    for (l = 0; l<ll; l++) g_gamma_r[u][l] = g_gamma[l][u];}
  g_uu = uu;
  g_ll = ll;}

/* allocModel - Allocates memory for a model containing uu states and ii
 *  features.
 */
Model *allocModel(int ii, int uu)
{ Model *m;
  int u;
  if (ii<=0||uu<=0) panic("allocModel(): %s must be>0", ii<=0?"ii":"uu");
  m = (Model *)safe_malloc(sizeof(Model));
  m->ffm = (Ffm **)safe_malloc(uu*sizeof(Ffm *));
  m->uu = uu;
  m->hmm = allocHMM(uu);
  for (u = 0; u<uu; u++) m->ffm[u] = allocFFM(ii);
  return m;}

/* copyModel - Copy all the contents from one model to another */
void copyModel(Model *dst_m, Model *src_m) {
  int u, i;
  assert(src_m&&dst_m);
  //assert(src_m->uu<=dst_m->uu); /* Take into account the elimination of unreachable states */
  assert(src_m->ffm[0]->ii==dst_m->ffm[0]->ii);

  if(src_m->uu != dst_m->uu){
    freeModel(dst_m);
    dst_m = allocModel(src_m->ffm[0]->ii, src_m->uu);
    for(i = 0; i < src_m->ffm[0]->ii; i ++){
      switch((src_m->ffm[0])->ft[i]){
      case FT_CONTINUOUS:
	defineContFeat(dst_m, i, ((struct ContFI*)((src_m->ffm[0])->fi[i]))->initialSigma);
	break;
      case FT_RADIAL:
	defineRadialFeat(dst_m, i);
	break;
      case FT_DISCRETE:
	defineDiscreteFeat(dst_m, i);
	break;
      default:break;
      }
    }
  }
  
  copyHMM(dst_m->hmm, src_m->hmm);
  for (u = 0; u<src_m->uu; u++) copyFFM(dst_m->ffm[u], src_m->ffm[u]);
}

void randomiseModel(Model *m, int ut, int rab)
{ int u;
  if (rab) randomiseHMM(m->hmm, ut);
  else constantHMM(m->hmm, ut);
  for (u = 0; u<m->uu; u++) randomiseParams(m->ffm[u]);}

/* defineContFeat - Defines feature i to be a continuous feature whose
 *  value is in the range (-sigma, sigma] with p = 0.68.  Non-zero mean
 *  features should be translated appropiately.
 */
void defineContFeat(Model *m, int i, Real sigma)
{ struct ContFI *fi;
  void *old;
  int u, ii = m->ffm[0]->ii;
  if (i<0||i>=ii)
  { panic("defineContFeat(): i = %d is outside range [0,%d]", i, ii-1);}
  if (sigma<=0.0) panic("defineContFeat(): sigma = %g must be>0.0", sigma);
  for (u = 0; u<m->uu; u++)
  { fi = (struct ContFI *)safe_malloc(sizeof(struct ContFI));
    fi->initialSigma = sigma;
    old = setFeatType(m->ffm[u], i, FT_CONTINUOUS, (void *)fi);
    if (old!=NULL) free(old);}}

/* defineRadialFeat - Defines feature i to be a radial feature whose
 *  value is in the range [0, 2*pi).
 */
void defineRadialFeat(Model *m, int i)
{ void *old;
  int u, ii = m->ffm[0]->ii;
  if (i<0||i>=ii)
  { panic("defineRadialFeat(): i = %d is outside range [0,%d]", i, ii-1);}
  for (u = 0; u<m->uu; u++)
  { old = setFeatType(m->ffm[u], i, FT_RADIAL, NULL);
    if (old!=NULL) free(old);}}

/* defineDiscreteFeat - Defines feature i to be a discrete feature whose
 *  value is in the range [0, n-1].
 */
void defineDiscreteFeat(Model *m, int i)
{ void *old;
  int u, ii = m->ffm[0]->ii;
  if (i<0||i>=ii)
  { panic("defineDiscreteFeat(): i = %d is outside range [0,%d]", i, ii-1);}
  for (u = 0; u<m->uu; u++)
  { old = setFeatType(m->ffm[u], i, FT_DISCRETE, NULL);
    if (old!=NULL) free(old);}}

void displayModel(FILE *f, Model *m)
{ int u;
  fprintf(f, "Model Display\n");
  displayHMM(f, m->hmm);
  for (u = 0; u<m->uu; u++)
  { fprintf(f, "-------\n");
    fprintf(f, "U = %3d\n", u);
    fprintf(f, "-------\n\n");
    displayFFM(f, m->ffm[u]);}}

void print_model(Model *m) {displayModel(stdout, m);}

void freeModel(Model *m)
{ int u;
  freeHMM(m->hmm);
  for (u = 0; u<m->uu; u++) freeFFM(m->ffm[u]);
  free(m);}

/* -- processing functions -- */

/* logLike -
 *  Calculates log likelihood of entire model given the feature data.
 */
Real logLike(Model *m, RMat data)
{ int u, uu = m->uu;
  RVec *logL;
  Real rv;
  logL = (RVec *)safe_malloc(uu*sizeof(RVec));
  for (u = 0; u<uu; u++)
    { assert(ROWS(data)==m->ffm[u]->ii);
      logL[u] = allocRVec(COLUMNS(data));
      FFM_logL(m->ffm[u], logL[u], data);}
  rv = HMM_logL(m->hmm, logL);
  for (u = 0; u<uu; u++) freeRVec(logL[u]);
  free(logL);
  /* needs work: Why is rv divided by the number of features? */
  return rv/model_ii(m);}

/* logLike_with_box_scores -
 *  Calculates log likelihood of entire model given the feature data.
 */
Real logLike_with_box_scores(Model *m, RMat data, RMat score_mat)
{ int u, uu = m->uu;
  RVec *logL;
  Real rv;
  Real **scores = MATRIX(score_mat);
  logL = (RVec *)safe_malloc(uu*sizeof(RVec));
  printf("1\n");
  for (u = 0; u<uu; u++)
    { //assert(ROWS(data)==m->ffm[u]->ii);
      //if(ROWS(data)!=m->ffm[u]->ii)
      //printf("@@@@@@@@ data->y = %d  m->ffm[u]->ii = %d\n", ROWS(data), m->ffm[u]->ii);
      logL[u] = allocRVec(COLUMNS(data));
      printf("2.1\n");
      FFM_logL_with_box_scores(m->ffm[u], logL[u], data,scores[u]);}
  printf("3\n");
  rv = HMM_logL(m->hmm, logL);
  for (u = 0; u<uu; u++) freeRVec(logL[u]);
  free(logL);
  /* needs work: Why is rv divided by the number of features? */
  return rv/model_ii(m);}

/* best_state_sequence -
 *  Calculates the best state sequence of entire model given the feature data.
 */
int *best_state_sequence(Model *m, RMat data)
{ int u, uu = m->uu, *rv;
  RVec *logL;
  logL = (RVec *)safe_malloc(uu*sizeof(RVec));
  for (u = 0; u<uu; u++)
  { assert(ROWS(data)==m->ffm[u]->ii);
    logL[u] = allocRVec(COLUMNS(data));
    FFM_logL(m->ffm[u], logL[u], data);}
  rv = HMM_best_state_sequence(m->hmm, logL);
  for (u = 0; u<uu; u++) freeRVec(logL[u]);
  free(logL);
  return rv;}

double** state_probabilities(Model *m, RMat data)
{ int u, uu = m->uu, t;
  RVec *gammas;
  RVec *logL;
  double **rv;
  logL = (RVec *)safe_malloc(uu*sizeof(RVec));
  gammas = (RVec *)safe_malloc(uu*sizeof(RVec));
  rv = (double **)safe_malloc(COLUMNS(data)*sizeof(double*));
  for (u = 0; u<uu; u++)
  { assert(ROWS(data)==m->ffm[u]->ii);
    logL[u] = allocRVec(COLUMNS(data));
    gammas[u] = allocRVec(COLUMNS(data));
    
    FFM_logL(m->ffm[u], logL[u], data);}  
  HMM_calcGammas(m->hmm,logL,gammas);

  for (t=0; t<COLUMNS(data); t++)
  { rv[t] = (double*)safe_malloc(uu*sizeof(double));
    for(u=0; u<uu; u++)
     { 
       rv[t][u] = VECTOR(gammas[u])[t];}}
  // copy the data into the array which is going to be passed to scheme
  for (u = 0; u<uu; u++) 
   { freeRVec(logL[u]);
     freeRVec(gammas[u]);}
  free(logL);
  free(gammas);
  return rv;}

double** state_probabilities_with_box_scores(Model *m, RMat data,
					     RMat score_mat)
{ int u, uu = m->uu, t;
  RVec *gammas;
  RVec *logL;
  double **rv;
  Real **scores = MATRIX(score_mat);
  logL = (RVec *)safe_malloc(uu*sizeof(RVec));
  gammas = (RVec *)safe_malloc(uu*sizeof(RVec));
  rv = (double **)safe_malloc(COLUMNS(data)*sizeof(double*));
  for (u = 0; u<uu; u++)
  { assert(ROWS(data)==m->ffm[u]->ii);
    logL[u] = allocRVec(COLUMNS(data));
    gammas[u] = allocRVec(COLUMNS(data));
    
    FFM_logL_with_box_scores(m->ffm[u], logL[u], data,scores[u]);}  
  HMM_calcGammas(m->hmm,logL,gammas);

  for (t=0; t<COLUMNS(data); t++)
  { rv[t] = (double*)safe_malloc(uu*sizeof(double));
    for(u=0; u<uu; u++)
     { 
       rv[t][u] = VECTOR(gammas[u])[t];}}
  // copy the data into the array which is going to be passed to scheme
  for (u = 0; u<uu; u++) 
   { freeRVec(logL[u]);
     freeRVec(gammas[u]);}
  free(logL);
  free(gammas);
  return rv;}
  

void force_init_globals(void) {g_lastData = NULL;}

void compute_posterior(Model **m, RMat *data, Real *prior, int *c_ls, int ll,
		       int cc, enum training_mode training_mode,
		       /* outputs */
		       Real *objective_function, Real *auxiliary, Real **postpC) {
  int l, c;
  Real postp_sum;
  assert(postpC);
  for (l = 0; l<ll; l++)
    for (c = 0; c<cc; c++)
      postpC[c][l] = logLike(m[c], data[l])+my_log(prior[c]);
  
  *objective_function = 0.0;
  *auxiliary = 0.0;
  for (l = 0; l<ll; l++) {
    postp_sum = NEGATIVE_INFINITY;
    for (c = 0; c<cc; c++) postp_sum = add_exp(postp_sum, postpC[c][l]);
    if(postp_sum==NEGATIVE_INFINITY)
      printf("|| A sample has -inf likelihood on all the models! \n || An outlier occurs (perhaps due to clip hack): l = %d\n", l);
    for (c = 0; c<cc; c++) {
	switch (training_mode) {
	case HMM_ML:
	  if(c==c_ls[l]) *objective_function += postpC[c][l];
	  if(postp_sum!=NEGATIVE_INFINITY)
	    postpC[c][l] -= postp_sum;
	  if(c==c_ls[l]) *auxiliary += postpC[c][l];
	  break;
	case HMM_DT:
	  if(c==c_ls[l]) *auxiliary += postpC[c][l];
	  if(postp_sum!=NEGATIVE_INFINITY)
	    postpC[c][l] -= postp_sum;
	  if(c==c_ls[l]) *objective_function += postpC[c][l];
	  break;
	default: panic("unrecognized training mode");
	}
    }
  }
}

/* update - Performs a single update on the HMM model for the given data. */
int update(Model **m, Hmm **tmp_hmm, RMat *data, Real **postpC, Real log_D,
	   int *c_ls, int ll, int cc, enum training_mode training_mode,
	   int upper_triangular) {
  /* tmp_hmm[c] is scratch space. It must have at least as many states as
     m[c]->hmm. */
  int c, l, u;
  int *xu;
  for (c = 0; c<cc; c++) {
    if (g_lastData!=data||ll>g_ll||m[c]->uu>g_uu) {
      g_lastData = data;
      initGlobals(ll, m[c]->uu, data);
    }
    zeroHMMlinear(tmp_hmm[c]);
    for (l = 0; l<ll; l++) {
      switch (training_mode) {
      case HMM_ML:
	if (c!=c_ls[l]) continue;
	for (u = 0; u<m[c]->uu; u++) {
	  FFM_logL(m[c]->ffm[u], g_logL[l][u], data[l]);
	}
	HMM_updateModel(m[c]->hmm, tmp_hmm[c], g_logL[l], g_gamma[l], log_D,
			0.0, -1, -1, training_mode);
	break;
      case HMM_DT:
	for (u = 0; u<m[c]->uu; u++) {
	  FFM_logL(m[c]->ffm[u], g_logL[l][u], data[l]);
	  //if(g_logL[l][u][tt-1] != NEGATIVE_INFINITY) flag = 1;
	}
	HMM_updateModel(m[c]->hmm, tmp_hmm[c], g_logL[l], g_gamma[l], log_D,
			my_exp(postpC[c][l]), c, c_ls[l], training_mode);
	break;
      default: panic("unrecognized training mode");
      }
    }
    
    xu = (int *)safe_malloc(sizeof(int) * m[c]->uu);
    if (!normaliseHMMlinear(tmp_hmm[c], upper_triangular, training_mode, xu)) {
      assert(training_mode == HMM_DT);
      free(xu);
      return FALSE;
    }
    
    copyHMM(m[c]->hmm, tmp_hmm[c]);
    
    for (u = 0; u<m[c]->uu; u++) {
      switch (training_mode) {
      case HMM_ML:
	assert(FFM_maximise(m[c]->ffm[u], data, g_gamma_r[u], ll, log_D,
			    NULL, c, c_ls));
	break;
      case HMM_DT:
	if (!FFM_maximise(m[c]->ffm[u], data, g_gamma_r[u], ll, log_D,
			  postpC[c], c, c_ls)){
	  free(xu);
	  return FALSE;
	}
	break;
      default: panic("unrecognized training mode");
      }
    }
    
    /* Remove redundant states by shifting the memory if necessary. */
    for(u = 0; u < m[c]->uu; u ++)
      if(xu[u]) break;
    if(u < m[c]->uu)
      removeStates(m[c], xu, tmp_hmm[c]);
       
    free(xu);
  }
  return TRUE;
}

int update_with_box_scores(Model **m, Hmm **tmp_hmm, RMat *data, Real **postpC, Real log_D,
	   int *c_ls, int ll, int cc, enum training_mode training_mode,
			   int upper_triangular, RMat *score_matrices) {
  /* tmp_hmm[c] is scratch space. It must have at least as many states as
     m[c]->hmm. */
  /* needs work: must call compute_posterior externally */
  int c, l, u;
  int *xu;
  Real** scores;
  for (c = 0; c<cc; c++) {
    if (g_lastData!=data||ll>g_ll||m[c]->uu>g_uu) {
      g_lastData = data;
      initGlobals(ll, m[c]->uu, data);
    }
    zeroHMMlinear(tmp_hmm[c]);
    for (l = 0; l<ll; l++) {
      scores = MATRIX(score_matrices[l]);
      switch (training_mode) {
      case HMM_ML:
	if (c!=c_ls[l]) continue;
	for (u = 0; u<m[c]->uu; u++) {
	  FFM_logL_with_box_scores(m[c]->ffm[u], g_logL[l][u], data[l],scores[u]);
	}
	HMM_updateModel(m[c]->hmm, tmp_hmm[c], g_logL[l], g_gamma[l], log_D,
			0.0, -1, -1, training_mode);
	break;
      case HMM_DT:
	for (u = 0; u<m[c]->uu; u++) {
	  FFM_logL_with_box_scores(m[c]->ffm[u], g_logL[l][u], data[l],scores[u]);
	}
	HMM_updateModel(m[c]->hmm, tmp_hmm[c], g_logL[l], g_gamma[l], log_D,
			my_exp(postpC[c][l]), c, c_ls[l], training_mode);
	break;
      default: panic("unrecognized training mode");
      }
    }

    xu = (int *)safe_malloc(sizeof(int) * m[c]->uu);
    if (!normaliseHMMlinear(tmp_hmm[c], upper_triangular, training_mode, xu)) {
      assert(training_mode == HMM_DT);
      free(xu);
      return FALSE;
    }

    /*if  !normaliseHMMlinear(tmp_hmm[c], upper_triangular)) {
        assert(training_mode == HMM_DT);
       
      return FALSE;
      }*/

    copyHMM(m[c]->hmm, tmp_hmm[c]);
    for (u = 0; u<m[c]->uu; u++) {
      switch (training_mode) {
      case HMM_ML:
	assert(FFM_maximise(m[c]->ffm[u], data, g_gamma_r[u], ll, log_D,
			    NULL, c, c_ls));
	break;
      case HMM_DT:
	if (!FFM_maximise(m[c]->ffm[u], data, g_gamma_r[u], ll, log_D,
			  postpC[c], c, c_ls))
	  return FALSE;
	break;
      default: panic("unrecognized training mode");
      }
    }
 
  }

  return TRUE;
}


/* -- accessor functions -- */
void removeStates(Model *m, int *xu, Hmm *hmm)
{
  int v, u, w, old_uu = m->uu;
  Hmm *phmm = m->hmm;
  for(u = 0; u < m->uu; ){
    if(xu[u]){
      if(u < m->uu - 1){
	for(v = u; v < m->uu - 1; v ++){
	  xu[v] = xu[v+1];
	  copyFFM(m->ffm[v], m->ffm[v+1]);
	  set_model_b(m, v, my_exp(VECTOR(phmm->logB)[v+1]));
	  VECTOR(hmm->logB)[v] = VECTOR(hmm->logB)[v+1];
	  copyRVec(MATRIX(phmm->logA)[v], MATRIX(phmm->logA)[v+1], m->uu);
	  copyRVec(MATRIX(hmm->logA)[v], MATRIX(hmm->logA)[v+1], m->uu);
	  for(w = 0; w < m->uu; w ++){
	    set_model_a(m, w, v, my_exp(MATRIX(phmm->logA)[w][v+1]));
	    MATRIX(hmm->logA)[w][v] = MATRIX(hmm->logA)[w][v+1];
	  }
	}
      }
      m->uu --;
      phmm->uu --;
      hmm->uu --;
    }
    else u ++;
  }
  assert(m->uu > 0);

  /* Clean up the redundant memory. DO NOT clean up hmm matrix now. It will be released 
     according to the size of RMat instead of hmm->uu. */
  for(u = m->uu; u < old_uu; u ++)
    freeFFM(m->ffm[u]);
}

int model_ii(Model *m)
{ assert(m!=NULL&&m->uu>0);
  return m->ffm[0]->ii;}

int model_feature_type(Model *m, int i)
{ assert(m!=NULL&&m->uu>0);
  if (i<0||i>=model_ii(m))
  { panic("model_feature_type(): i = %d out of range [0, %d]",
	  i, model_ii(m));}
  return m->ffm[0]->ft[i];}

Real model_parameter(Model *m, int u, int i, int n)
{ assert(m!=NULL&&m->uu>0);
  if (i<0||i>=model_ii(m))
  { panic("model_parameter(): i = %d out of range [0, %d]", i, model_ii(m));}
  if (u<0||u>=m->uu)
  { panic("model_parameter(): u = %d out of range [0, %d]", u, m->uu);}
  switch (m->ffm[u]->ft[i])
  { case FT_CONTINUOUS:
    switch (n)
    { case 0: return ((Param *)m->ffm[u]->p[i])->mu;
      case 1: return ((Param *)m->ffm[u]->p[i])->sigma;
      default: panic("model_parameter(): n = %d out of range [0, 1]", n);}
    case FT_RADIAL:
    switch (n)
    { case 0: return ((VMParam *)m->ffm[u]->p[i])->mean;
      case 1: return ((VMParam *)m->ffm[u]->p[i])->kappa;
      default: panic("model_parameter(): n = %d out of range [0, 1]", n);}
    case FT_DISCRETE:
    switch (n)
    { case 0: return ((DParam *)m->ffm[u]->p[i])->kk;
      default:
	if (n-1<0||n-1>=((DParam *)m->ffm[u]->p[i])->kk)
	  panic("model_parameter(): n = %d out of range [0, n]", n);
      return ((DParam *)m->ffm[u]->p[i])->p[n-1];}
    default:
    panic("model_parameter(): Unrecognised feature type: %d",
	  m->ffm[u]->ft[i]);}
  panic("model_parameter(): Control shouldn't reach this point");
  return 0.0;}

int model_uu(Model *m)
{ assert(m!=NULL);
  return m->uu;}

Real model_a(Model *m, int u, int v)
{ assert(m!=NULL&&m->hmm!=NULL);
  if (u<0||u>=m->uu||v<0||v>=m->uu)
  { panic("model_a(): u = %d or v = %d out of range [0, %d]", u, v, m->uu);}
  return my_exp(MATRIX(m->hmm->logA)[u][v]);}

Real model_b(Model *m, int u)
{ assert(m!=NULL&&m->hmm!=NULL);
  if (u<0||u>=m->uu) panic("model_b(): u = %d out of range [0, %d]", u, m->uu);
  return my_exp(VECTOR(m->hmm->logB)[u]);}

void set_model_parameter(Model *m, int u, int i, int n, Real x)
{ int j;
  assert(m!=NULL&&m->uu>0);
  if (i<0||i>=model_ii(m))
  { panic("set_model_parameter(): i = %d out of range [0, %d]",
	  i, model_ii(m));}
  if (u<0||u>=m->uu)
  { panic("set_model_parameter(): u = %d out of range [0, %d]", u, m->uu);}
  if (m->ffm[u]->p[i]==NULL)
  { panic("set_model_parameter(): feature %d is not initialised", i);}
  switch (m->ffm[u]->ft[i])
  { case FT_CONTINUOUS:
    switch (n)
    { case 0:
      ((Param *)m->ffm[u]->p[i])->mu = x;
      break;
      case 1:
      if (x<=0.0)
      {
	/* debugging ANDREI */
	/* panic("set_model_parameter(): x = %f is out of range (0, inf)", x); */
	((Param *)m->ffm[u]->p[i])->sigma = 0.01;
      } else { ((Param *)m->ffm[u]->p[i])->sigma = x; }
      break;
      default:
      panic("set_model_parameter(): n = %d out of range [0, 1]", n);}
    break;
    case FT_RADIAL:
    switch (n)
    { case 0:
      ((VMParam *)m->ffm[u]->p[i])->mean = x;
      break;
      case 1:
      if (x<=0.0)
      {
	/* debugging ANDREI */
	/* panic("set_model_parameter(): x = %f is out of range (0, inf)", x); */
	((VMParam *)m->ffm[u]->p[i])->kappa = 0.01;
      } else { ((VMParam *)m->ffm[u]->p[i])->kappa = x; }
      break;
      default:
      panic("set_model_parameter(): n = %d out of range [0, 1]", n);}
    break;
    case FT_DISCRETE:
    switch (n)
    { case 0:
      j = (int)round(x);
      if (j<=0||j>MAX_DISCRETE)
	panic("set_model_parameter(): x = %f is out of range [1, MAX_DISCRETE]", x);
      ((DParam *)m->ffm[u]->p[i])->kk = j;
      break;
      default:
      if (n-1<0||n-1>=((DParam *)m->ffm[u]->p[i])->kk)
	panic("model_parameter(): n-1 = %d out of range [0, kk - 1]", n-1);
      if (x<0.0||x>1.0)
      { panic("set_model_parameter(): x = %f is out of range [0, 1]", x);}
      ((DParam *)m->ffm[u]->p[i])->p[n-1] = x; }
    break;
    default:
    panic("set_model_paramter(): Unrecognised feature type: %d, %d, %d",
	  m->ffm[u]->ft[i], u, i);}}

void set_model_a(Model *m, int u, int v, Real x)
{ assert(m!=NULL&&m->hmm!=NULL);
  if (u<0||u>=m->uu||v<0||v>=m->uu)
  { panic("set_model_a(): u = %d or v = %d out of range [0, %d]",
	  u, v, m->uu);}
  if (x<0.0||x>1.0) panic("set_model_a(): x = %f is out of range [0, 1]", x);
  MATRIX(m->hmm->logA)[u][v] = my_log(x);}

void set_model_b(Model *m, int u, Real x)
{ assert(m!=NULL&&m->hmm!=NULL);
  if (u<0||u>=m->uu)
  { panic("set_model_b(): u = %d out of range [0, %d]", u, m->uu);}
  if (x<0.0||x>1.0) panic("set_model_b(): x = %f is out of range [0, 1]", x);
  VECTOR(m->hmm->logB)[u] = my_log(x);}

/* Tam V'Nishlam Shevah L'El Borei Olam */
