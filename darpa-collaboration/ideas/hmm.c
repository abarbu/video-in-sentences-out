/* LaHaShem HaAretz U'Mloah */

/* Program to train and evaluate hidden Markov models */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include <stdarg.h>
#include <string.h>
#include "hmm.h"
#include "hmm-rand.h"
#include "hmm-data.h"
#include "hmm-control.h"

static RMat g_alpha = NULL;
static RMat g_delta = NULL;
static RMat g_deltav = NULL;	/* This could be int. */
static RMat g_beta = NULL;
static RMat3d g_psi = NULL;

/* allocHMM - Allocates space for an HMM and returns a pointer to it. */
Hmm *allocHMM(int uu)
{ Hmm *m = (Hmm *)safe_malloc(sizeof(Hmm));
  assert(uu>0);
  m->logA = allocRMat(uu, uu);
  m->logB = allocRVec(uu);
  m->uu = uu;
  return m;}

/* constantHMM -
 *  Initialises an HMM with constant transition log probabilities.
 */
void constantHMM(Hmm *m, int upper_triangular)
{ int u, v;
  zeroHMM(m);
  for (u = 0; u<m->uu; u++)
  { VECTOR(m->logB)[u] = 0;
    for (v = upper_triangular?u:0; v<m->uu; v++) MATRIX(m->logA)[u][v] = 0;}
  normaliseHMM(m, upper_triangular);}

void copyHMM(Hmm *m1, Hmm *m2)
{ int u;
  //assert(m1->uu>=m2->uu);   /* Take into account the elimination of unreachable states */

  if(m1->uu != m2->uu){
    freeHMM(m1);
    m1 = allocHMM(m2->uu);
  }

  copyRVec(VECTOR(m1->logB), VECTOR(m2->logB), m1->uu);
  for (u = 0; u<m1->uu; u++)
  { copyRVec(MATRIX(m1->logA)[u], MATRIX(m2->logA)[u], m1->uu);}
}

/* displayHMM - Writes the HMM `m' in a human-readable format to output
 *  stream `f'.  Only displays a maximum of DISPLAY_HMM_MAX_U states
 *  on a single line.
 */
void displayHMM(FILE *f, Hmm *m)
{ int i, u;
  /* display B vector */
  fprintf(f, "%8s", "");
  for (u = 0; u<DISPLAY_HMM_MAX_U&&u<m->uu; u++) fprintf(f, " %4d", u);
  fprintf(f, "\n");
  fprintf(f, "HMM:  B:");
  for (u = 0; u<DISPLAY_HMM_MAX_U&&u<m->uu; u++)
  { fprintf(f, " %4.2f", my_exp(VECTOR(m->logB)[u]));}
  fprintf(f, "\n");
  for (;u<m->uu; u++)
  { fprintf(f, "\n");
    fprintf(f, "%8s", "");
    for (i = 0; i<DISPLAY_HMM_MAX_U&&i+u<m->uu; u++, i++) {
      fprintf(f, " %4d", i+u);
    }
    fprintf(f, "\n");
    for (i = 0; i<DISPLAY_HMM_MAX_U&&u<m->uu; u++, i++)
    { fprintf(f, " %4.2f", my_exp(VECTOR(m->logB)[u]));}
    fprintf(f, "\n");}
  /* display A matrix */
  fprintf(f, "%8s", "A:");
  for (u = 0; u<DISPLAY_HMM_MAX_U&&u<m->uu; u++) fprintf(f, " %4d", u);
  fprintf(f, "\n");
  for (u = 0; u<DISPLAY_HMM_MAX_U&&u<m->uu; u++)
  { fprintf(f, "%8d", u);
    for (i = 0; i<DISPLAY_HMM_MAX_U&&i<m->uu; i++)
    { fprintf(f, " %4.2f", my_exp(MATRIX(m->logA)[u][i]));}
    fprintf(f, "\n");}}

/* freeHMM - Frees a previously allocated HMM. */
void freeHMM(Hmm *m)
{ freeRMat(m->logA);
  freeRVec(m->logB);
  free(m);}

/* normaliseHMM -
 *  Normalises an HMM so that the transition log probabilities from each
 *  state sum to zero.
 *  Basically NOT USED now.
 */
void normaliseHMM(Hmm *m, int upper_triangular)
{ int u, v;
  Real sum;
  /* this deals with unreachable states */
  Real delta_correction = my_log(1.0e-300);
  sum = sumOfLogs(VECTOR(m->logB), m->uu);
  assert(sum>NEGATIVE_INFINITY);
  for (u = 0; u<m->uu; u++) VECTOR(m->logB)[u] -= sum;
  for (u = 0; u<m->uu; u++)
  { sum = sumOfLogs(MATRIX(m->logA)[u], m->uu);
    if (sum == NEGATIVE_INFINITY)
      { for (v = upper_triangular?u:0; v<m->uu; v++)
	{ MATRIX(m->logA)[u][v] = delta_correction; }
	sum = sumOfLogs(MATRIX(m->logA)[u], m->uu);}
    for (v = 0; v<m->uu; v++) { MATRIX(m->logA)[u][v] -= sum;}}}

/* normaliseHMMlinear -
    Normalises an HMM in the original space so that transition probabilites
    sum to 1. After this, convert all the values from the original space to
    log space. This function should be paired with zeroHMMlinear. If some value
    in the numerator is negative while the sum is positive, return false which
    means should increase log_D; otherwise return true.
 */
int normaliseHMMlinear(Hmm *m, int upper_triangular, enum training_mode train_mode, int *xu)
{ int u, v, w;
  Real sum, corrected_sum;
  Real delta_correction = 1e-20;
  int flag;
  sum = 0.0;
  for (u = 0; u<m->uu; u++){
    //printf("b0[%d]=%e ", u, VECTOR(m->logB)[u]);
    sum += VECTOR(m->logB)[u];
  }
  //printf("\n");
  assert(fabs(sum)>0.0);
  flag = 0;
  corrected_sum = NEGATIVE_INFINITY;
  for (u = 0; u<m->uu; u++) {
    if(VECTOR(m->logB)[u]/sum < 0.0){
      //printf("@@@ b[%d] = %0.10lf sum = %0.4lf\n", u, VECTOR(m->logB)[u], sum);
      return FALSE;
    }
    VECTOR(m->logB)[u] = my_log(VECTOR(m->logB)[u]/sum);
    /* Clip the value to zero if it is smaller than delta_correction
       This jumps to another hopefully better optimization space */
    if(train_mode == HMM_DT && VECTOR(m->logB)[u] != NEGATIVE_INFINITY 
       && VECTOR(m->logB)[u] < my_log(delta_correction)){
      printf("b[%d]=%e is replaced by 0.0!\n", u, my_exp(VECTOR(m->logB)[u]));
      VECTOR(m->logB)[u] = NEGATIVE_INFINITY;
      flag = 1;
    }
    corrected_sum = add_exp(corrected_sum, VECTOR(m->logB)[u]);
  }
  /* Normalise the corrected array is necessary */
  if(flag){
    for(u = 0; u < m->uu; u ++)
      VECTOR(m->logB)[u] -= corrected_sum;
  }

  for (u = 0; u<m->uu; u++)
  { sum = 0.0;
    w = (upper_triangular?u:0);
    for (v = w; v<m->uu; v++) sum += MATRIX(m->logA)[u][v];
    if (sum==0.0) {
      /* A state never jumps to another state (include itself)
	 This is not the unreachable state. It may be a final state or a useless state.
	 An unreachable state is finally to be a useless state.
         We can just keep all the outward transition probabilities zero. */
      // Do NOTHING. We don't want a corrected normal distribution. 
      for(v = 0; v < m->uu; v ++)
	MATRIX(m->logA)[u][v] = NEGATIVE_INFINITY;
      continue;
    }
    flag = 0;
    corrected_sum = NEGATIVE_INFINITY;
    for (v = 0; v<m->uu; v ++) {
      if(MATRIX(m->logA)[u][v]/sum < 0.0){
	//printf("@@@ m->A[%d][%d] = %0.4lf   sum = %0.4lf\n", u, v, MATRIX(m->logA)[u][v], sum);
	return FALSE;
      }
      MATRIX(m->logA)[u][v] = my_log(MATRIX(m->logA)[u][v]/sum);
      /* Clip the value to zero if it is smaller than delta_correction
       This jumps to another hopefully better optimization space */
      if(train_mode == HMM_DT && MATRIX(m->logA)[u][v] != NEGATIVE_INFINITY 
	 && MATRIX(m->logA)[u][v] < my_log(delta_correction)){
	//printf("a[%d][%d]=%e is replaced by 0.0!\n", u, v, my_exp(MATRIX(m->logA)[u][v]));
	MATRIX(m->logA)[u][v] = NEGATIVE_INFINITY;
	flag = 1;
      }
      corrected_sum = add_exp(corrected_sum, MATRIX(m->logA)[u][v]);
    }
    if(flag){
      for(v = 0; v < m->uu; v ++)
	MATRIX(m->logA)[u][v] -= corrected_sum;
    }
  }

  /* Check whether any column in logA has entries that are all NEGATIVE_INFINITY,
     which is an unreachable state. */
  memset(xu, 0, sizeof(int) * m->uu);
  for(u = 0; u < m->uu; u ++){
    flag = 0;
    if(VECTOR(m->logB)[u] == NEGATIVE_INFINITY) flag ++;
    for(v = 0; v < m->uu; v ++)
      if(MATRIX(m->logA)[v][u] != NEGATIVE_INFINITY) break;
    if(v == m->uu) flag ++;
    if(flag == 2){
      //printf("AN UNREACHABLE STATE IS TO BE REMOVED! State_#: %d\n", u);
      xu[u] = 1;
    }
  }
   
  return TRUE;
}

/* randomiseHMM -
 *  Initialises an HMM with normalised random transition log probabilities.
 */
void randomiseHMM(Hmm *m, int upper_triangular)
{ int u, v;
  zeroHMM(m);
  for (u = 0; u<m->uu; u++)
  { VECTOR(m->logB)[u] = my_log(RANDOM);
    for (v = upper_triangular?u:0; v<m->uu; v++)
    { MATRIX(m->logA)[u][v] = my_log(RANDOM);}}
  normaliseHMM(m, upper_triangular);}

/* zeroHMM - Sets all of the transition probabilities to 0.0. */
void zeroHMM(Hmm *m)
{ int u, v;
  for (u = 0; u<m->uu; u++)
  { VECTOR(m->logB)[u] = NEGATIVE_INFINITY;
    for (v = 0; v<m->uu; v++)
    { MATRIX(m->logA)[u][v] = NEGATIVE_INFINITY;}}}

/* zeroHMMlinear - Sets all of the transition probabilities to 0.0 in the
   orginal space. This function is needed in dt training because during update a
   negative value may occur. It should be used with normaliseHMMlinear. */
void zeroHMMlinear(Hmm *m)
{ int u, v;
  for (u = 0; u<m->uu; u++)
  { VECTOR(m->logB)[u] = 0.0;
    for (v = 0; v<m->uu; v++)
    { MATRIX(m->logA)[u][v] = 0.0;}}}

/* -- processing functions -- */

/* HMM_calcAlphas -
 *  Given an HMM and a vector of log likelihoods for states assigned to
 *  data vectors in the sequence, calculates the alpha values from the
 *  forward-backward algorithm.
 */
void HMM_calcAlphas(Hmm *m, RVec *logL)
{ int t, u, v, tt = VLENGTH(logL[0]);
  Real **al, **a = MATRIX(m->logA), *b = VECTOR(m->logB);
  HMM_initGlobals(m->uu, tt);
  al = MATRIX(g_alpha);
  /* alpha[u][1] = b[u]*logL[u][1]
   * alpha[u][t] = sum(v = 1 ... m->uu, a[v][u]*alpha[v][t-1])*logL[u][t]
   */
  for (u = 0; u<m->uu; u++){
    al[u][0] = VECTOR(logL[u])[0]+b[u];
    //if(u == 1)
    //  printf("####cal_al ==> logL[%d][0]=%0.5lf logB[%d]=%0.5lf\n", u, VECTOR(logL[u])[0], u, b[u]);
  }
  for (t = 1; t<tt; t++)
  { for (u = 0; u<m->uu; u++)
    { al[u][t] = NEGATIVE_INFINITY;
      for (v = 0; v<m->uu; v++)
      { al[u][t] = add_exp(al[u][t], a[v][u]+al[v][t-1]);}
      al[u][t] += VECTOR(logL[u])[t];
    }
  }
}

/* HMM_calcDeltas -
 *  Given an HMM and a vector of log likelihoods for states assigned to
 *  data vectors in the sequence, calculates the delta values from the
 *  forward-backward algorithm.
 */
void HMM_calcDeltas(Hmm *m, RVec *logL)
{ int t, u, v, tt = VLENGTH(logL[0]);
  /* dev could be int. */
  Real **de, **dev, **a = MATRIX(m->logA), *b = VECTOR(m->logB), d;
  HMM_initGlobals(m->uu, tt);
  de = MATRIX(g_delta);
  dev = MATRIX(g_deltav);	/* This could be int. */
  /* delta[u][1] = b[u]*logL[u][1]
   * delta[u][t] = max(v = 1 ... m->uu, a[v][u]*delta[v][t-1])*logL[u][t]
   */
  for (u = 0; u<m->uu; u++) de[u][0] = VECTOR(logL[u])[0]+b[u];
  for (t = 1; t<tt; t++)
  { for (u = 0; u<m->uu; u++)
    { de[u][t] = NEGATIVE_INFINITY;
      dev[u][t] = 0;		/* This could be int. */
      for (v = 0; v<m->uu; v++)
      { d = a[v][u]+de[v][t-1];
	if (d>de[u][t])
	{ de[u][t] = d;
	  dev[u][t] = v;}}	/* This could be int. */
      de[u][t] += VECTOR(logL[u])[t];}}}

/* HMM_initGlobals -
 *  Given a number of states and a sequence length, allocates or reallocates
 *  memory for internal variables.
 */
void HMM_initGlobals(int uu, int tt)
{ if (g_alpha==NULL||COLUMNS(g_alpha)<tt||ROWS(g_alpha)<uu)
  { if (g_alpha!=NULL)
    { freeRMat(g_alpha);
      freeRMat(g_delta);
      freeRMat(g_deltav);	/* This could be int. */
      freeRMat(g_beta);
      freeRMat3d(g_psi);}
    g_alpha = allocRMat(uu, tt);
    g_delta = allocRMat(uu, tt);
    g_deltav = allocRMat(uu, tt); /* This could be int. */
    g_beta = allocRMat(uu, tt);
    g_psi = allocRMat3d(uu, uu, tt);}}

/* HMM_logL -
 *  Given an HMM and a vector of log likelihoods for states assigned to
 *  data vectors in the sequence, returns the log likelihood of the data
 *  given the HMM.
 */
Real HMM_logL(Hmm *m, RVec *logL)
{ int u, tt = VLENGTH(logL[0]);
  Real l = NEGATIVE_INFINITY;
  /* P(sequence|m) = sum(u = 1 ... m->uu, alpha[u][tt]) */
  HMM_calcAlphas(m, logL);
  for (u = 0; u<m->uu; u++) l = add_exp(l, MATRIX(g_alpha)[u][tt-1]);
  return l;}

/* HMM_best_state_sequence -
 *  Given an HMM and a vector of log likelihoods for states assigned to
 *  data vectors in the sequence, returns the best state sequence of the data
 *  given the HMM.
 */
int *HMM_best_state_sequence(Hmm *m, RVec *logL)
{ int u, t, tt = VLENGTH(logL[0]), *rv;
  Real b, d;
  rv = (int *)safe_malloc(tt*sizeof(int));
  HMM_calcDeltas(m, logL);
  b = NEGATIVE_INFINITY;
  rv[tt-1] = 0;
  for (u = 0; u<m->uu; u++)
  { d = MATRIX(g_delta)[u][tt-1];
    if (d>b)
    { b = d;
      rv[tt-1] = u;}}
  /* This could be int. */
  for (t = tt-2; t>=0; t--) rv[t] = MATRIX(g_deltav)[rv[t+1]][t+1];
  return rv;}

/* HMM_calcGammas computes gammas, which are the probabilities that
 * each frame is in each state
 */
void HMM_calcGammas(Hmm *m, RVec *logL, RVec *gamma) {
  int t, u, v, tt = VLENGTH(logL[0]);
  Real **a = MATRIX(m->logA), *b = VECTOR(m->logB), **al, **be, ***ps;
  Real logD;
  Real like;

  assert(VLENGTH(logL[0])==VLENGTH(gamma[0]));
  HMM_initGlobals(m->uu, tt);
  al = MATRIX(g_alpha);
  be = MATRIX(g_beta);
  ps = MATRIX(g_psi);
  /* calculate alpha's */
  HMM_calcAlphas(m, logL);
  /* calculate beta's -
   * beta[u][tt] = 1
   * beta[u][t] = sum(v = 1 ... m->uu, a[u][v]*beta[v][t+1]*logL[v][t+1])
   */
  for (u = 0; u<m->uu; u++) be[u][tt-1] = 0.0;
  for (t = tt-2; t>=0; t--)
    { for (u = 0; u<m->uu; u++)
	{ be[u][t] = NEGATIVE_INFINITY;
	  for (v = 0; v<m->uu; v++)
	    { be[u][t] =
		add_exp(be[u][t], a[u][v]+be[v][t+1]+VECTOR(logL[v])[t+1]);}}}
  /* calculate logL of sequence -
   * P(sequence|m) = sum(u = 1 ... m->uu, alpha[u][tt])
   */
  like = NEGATIVE_INFINITY;
  for (u = 0; u<m->uu; u++) like = add_exp(like, al[u][tt-1]);
  /* calculate responsibilities
   *               alpha[u][t]*beta[u][t]
   * gamma[u][t] = ----------------------
   *                    P(data|model)
   */
  for (t = 0; t<tt; t++)
    { for (u = 0; u<m->uu; u++) VECTOR(gamma[u])[t] = al[u][t]+be[u][t]-like;}}

/* HMM_updateModel -
 *  Given an HMM and a vector of log likelihoods for states in the sequences,
 *  calculates the responsibilities of each state in the HMM for each symbol
 *  in the sequences, and maximises the model parameters based on the
 *  assigned log likelihoods.
*/
Real HMM_updateModel(Hmm *m, Hmm *new, RVec *logL, RVec *gamma, Real log_D, 
		     Real postpC, int c, int c_ls,
		     enum training_mode training_mode) {
  int t, u, v, tt = VLENGTH(logL[0]);
  Real **a = MATRIX(m->logA), *b = VECTOR(m->logB), **al, **be, ***ps;
  Real logD, like, dtf;
  int Sc = (c==c_ls);
  switch (training_mode) {
  case HMM_ML:
    assert(postpC==0.0);	/* needs work: ask yu239 */
    logD = NEGATIVE_INFINITY;
    break;
  case HMM_DT:
    assert(c>=0&&c_ls>=0);
    logD = log_D;
    break;
  default: panic("unrecognized training mode");
  }
  assert(VLENGTH(logL[0])==VLENGTH(gamma[0]));
  HMM_initGlobals(m->uu, tt);
  al = MATRIX(g_alpha);
  be = MATRIX(g_beta);
  ps = MATRIX(g_psi);
  /* calculate alpha's */
  HMM_calcAlphas(m, logL);
  /* calculate beta's -
   * beta[u][tt] = 1
   * beta[u][t] = sum(v = 1 ... m->uu, a[u][v]*beta[v][t+1]*logL[v][t+1])
   */
  for (u = 0; u<m->uu; u++) be[u][tt-1] = 0.0;
  for (t = tt-2; t>=0; t--)
  { for (u = 0; u<m->uu; u++)
    { be[u][t] = NEGATIVE_INFINITY;
      for (v = 0; v<m->uu; v++)
      { be[u][t] =
	add_exp(be[u][t], a[u][v]+be[v][t+1]+VECTOR(logL[v])[t+1]);}}}
  
  /* calculate logL of sequence -
   * P(sequence|m) = sum(u = 1 ... m->uu, alpha[u][tt])
   */
  like = NEGATIVE_INFINITY;
  for (u = 0; u<m->uu; u++)
    like = add_exp(like, al[u][tt-1]);

  /* A sample that can NEVER belong to this category */
  if(like == NEGATIVE_INFINITY){
    if(postpC != 0.0)
      printf("Like inf but postpC(%e) != 0.0!\n", postpC);
    assert(postpC == 0.0);
    assert(Sc==0);
  }

  /* calculate responsibilities
   *               alpha[u][t]*beta[u][t]
   * gamma[u][t] = ----------------------
   *                    P(data|model)
   */
  for (t = 0; t<tt; t++){
     for (u = 0; u<m->uu; u++){ 
       if(like!=NEGATIVE_INFINITY)
	 VECTOR(gamma[u])[t] = al[u][t]+be[u][t]-like;
       else
	 VECTOR(gamma[u])[t] = NEGATIVE_INFINITY;
     }
  }
  /* calculate time-indexed transition probabilities
   *                alpha[u][t]*a[u][v]*logL[v][t+1]*beta[v][t+1]
   * psi[u][v][t] = ---------------------------------------------
   *                               P(data|model)
   */
  for (u = 0; u<m->uu; u++){ 
    for (v = 0; v<m->uu; v++){ 
      for (t = 0; t<tt-1; t++){ 
	if(like!=NEGATIVE_INFINITY)
	  ps[u][v][t] = al[u][t]+a[u][v]+VECTOR(logL[v])[t+1]+be[v][t+1]-like;
	else
	  ps[u][v][t] = NEGATIVE_INFINITY;
      }
    }
  }
  /* Update new model. The model may have been partly updated by some training
     samples. */
  a = MATRIX(new->logA);
  b = VECTOR(new->logB);
  /* calculate B
     b[u] = gamma[u][1]
     - added scaling by sum of gammas to catch any numerical accuracy problems
     NO LOG-SPACE HERE !
   */
  Real tmp;
  for (u = 0; u<m->uu; u++) {
    /* This may be negative */
    //tmp = (Sc-postpC)*my_exp(VECTOR(gamma[u])[0])
    //  +my_exp(logD+VECTOR(m->logB)[u]);
    b[u] += (Sc-postpC)*my_exp(VECTOR(gamma[u])[0])
      +my_exp(logD+VECTOR(m->logB)[u]);
    /*if(tmp < 0){
      for(int ii = 0; ii < m->uu; ii ++)
	printf("==>Sc:%d c=%d u=%d postpC=%0.10lf gamma[u][0]:%0.10lf D:%0.10lf old_b[u]:%0.10lf\n", Sc, c, ii, postpC, my_exp(VECTOR(gamma[ii])[0]), my_exp(logD), my_exp(VECTOR(m->logB)[ii]));
	}*/
  }
  /* calculate A matrix
   *                    sum(t = 1 ... tt-1, psi[u][v][t])
   * a[u][v] = -------------------------------------------------------
   *           sum(t = 1 ... tt-1, sum(w = 1 ... m->uu, psi[u][w][t]))
   * see note above about log space
   */
  for (u = 0; u<m->uu; u++) {
    for (v = 0; v<m->uu; v++) {
      /* This may be negative */
      dtf = 0.0;
      for(t = 0; t<tt-1; t++)
	dtf += my_exp(ps[u][v][t])*(Sc-postpC) + my_exp(logD+MATRIX(m->logA)[u][v]); 
      a[u][v] += dtf;
    }
  }

  for (t = 0; t<tt; t++) {
    for (u = 0; u<m->uu; u++) VECTOR(gamma[u])[t] = my_exp(VECTOR(gamma[u])[t]);
  }
  return like;
}

/* needs work : maybe shouldn't check exact equality with NEGATIVE_INFINITY */
Real add_exp(Real e1, Real e2)
{ Real e_max = MAX(e1, e2), e_min = MIN(e1, e2);
  return (e_max==NEGATIVE_INFINITY)? NEGATIVE_INFINITY:
    (e_max-e_min>LOG_MATH_PRECISION)? e_max:
    my_log(1.0+my_exp(e_min-e_max))+e_max;}

Real my_exp(Real x)
{ if (x==NEGATIVE_INFINITY) return 0.0;
  return exp(x);}

Real my_log(Real x)
{ if (x==0.0) return NEGATIVE_INFINITY;
  else return log(x);}

Real my_atan2(Real x, Real y)
{ if (x==0.0&&y==0.0) return 0.0;
  else if (x==0.0&&y==-1.0) return -PI;
  else return atan2(x, y);}

RMat *allocate_rmat_vector(size_t n)
{ RMat *v = (RMat *)safe_malloc(sizeof(RMat)*n);
  return v;}

RVec allocRVec(size_t x)
{ RVec v = (RVec)safe_malloc(sizeof(struct RVecStruct));
  VECTOR(v) = (Real *)safe_malloc(x*sizeof(Real));
  VLENGTH(v) = x;
  return v;}

RMat allocRMat(size_t y, size_t x)
{ int i;
  RMat m = (RMat)safe_malloc(sizeof(struct RMatStruct));
  MATRIX(m) = (Real **)safe_malloc(y*sizeof(Real *));
  for (i = 0; i<y; i++) MATRIX(m)[i] = (Real *)safe_malloc(x*sizeof(Real));
  ROWS(m) = y;
  COLUMNS(m) = x;
  return m;}

RMat3d allocRMat3d(size_t z, size_t y, size_t x)
{ int i, j;
  RMat3d m = (RMat3d)safe_malloc(sizeof(struct RMat3dStruct));
  MATRIX(m) = (Real ***)safe_malloc(z*sizeof(Real **));
  for (i = 0; i<z; i++)
  { MATRIX(m)[i] = (Real **)safe_malloc(y*sizeof(Real *));
    for (j = 0; j<y; j++)
    { MATRIX(m)[i][j] = (Real *)safe_malloc(x*sizeof(Real));}}
  m->z = z;
  m->y = y;
  m->x = x;
  return m;}

void free_rmat_vector(RMat *v) {free(v);}

void freeRVec(RVec v)
{ if (v!=NULL)
  { if (VECTOR(v)!=NULL) free(VECTOR(v));
    free(v);}}

void freeRMat(RMat m)
{ int i;
  if (m!=NULL)
  { if (MATRIX(m)!=NULL)
    { for (i = 0; i<ROWS(m); i++)
      { if (MATRIX(m)[i]!=NULL) free(MATRIX(m)[i]);}
      free(MATRIX(m));}
    free(m);}}

void freeRMat3d(RMat3d m)
{ int i, j;
  if (m!=NULL)
  { if (MATRIX(m)!=NULL)
    { for (i = 0; i<m->z; i++)
      { if (MATRIX(m)[i]!=NULL)
	{ for (j = 0; j<m->y; j++)
	  { if (MATRIX(m)[i][j]!=NULL) free(MATRIX(m)[i][j]);}
	  free(MATRIX(m)[i]);}}
      free(MATRIX(m));}
    free(m);}}

/* vector operations */

Real *addRVec(Real *u, const Real *v, int size)
{ for (size--; size>=0; size--) u[size] += v[size];
  return u;}

Real *copyRVec(Real *u, const Real *v, int size)
{ for (size--; size>=0; size--) u[size] = v[size];
  return u;}

Real dotProdRVec(const Real *u, const Real *v, int size)
{ Real x = 0.0;
  for (size--; size>=0; size--) x += u[size]*v[size];
  return x;}

/* normaliseRVec -
 * scales a vector so that the values in the vector lie in the range of
 * -1 to 1.  The scaled vector will only contain positive (or negative) values
 * if the original vector contained positive (or negative) values.  Returns
 * the maximum absolute value in the vector */

Real normaliseRVec(Real *v, int size)
{ int i;
  Real max = (size>0)?fabs(v[0]):0.0;
  for (i = 1; i<size; i++) if (fabs(v[i])>max) max = fabs(v[i]);
  if (max!=0.0) for (i = 0; i<size; i++) v[i] /= max;
  return max;}

Real rmat_get(RMat rmat, int i, int j)
{ if (i<0||i>=ROWS(rmat)) panic("i=%d out of bounds (0,%d)", i, ROWS(rmat));
  if (j<0||j>=COLUMNS(rmat))
  { panic("j=%d out of bounds (0,%d)", i, COLUMNS(rmat));}
  return MATRIX(rmat)[i][j];}

void rmat_set(RMat rmat, int i, int j, Real x)
{ if (i<0||i>=ROWS(rmat)) panic("i=%d out of bounds (0,%d)", i, ROWS(rmat));
  if (j<0||j>=COLUMNS(rmat))
  { panic("j=%d out of bounds (0,%d)", i, COLUMNS(rmat));}
  MATRIX(rmat)[i][j] = x;}

void rmat_vector_set(RMat *v, int i, RMat r) {v[i] = r;}

/* scaleRVec - v = k*v */

Real *scaleRVec(Real *v, Real k, int size)
{ for (size--; size>=0; size--) v[size] *= k;
  return v;}

/* sumOfLogs - x = log(exp(v[1]) + exp(v[2]) + ... + exp(v[size])) */

Real sumOfLogs(Real *v, int size)
{ Real x = NEGATIVE_INFINITY;
  for (size--; size>=0; size--) x = add_exp(x, v[size]);
  return x;}

Real sumRVec(Real *v, int size)
{ Real x = 0.0;
  for (size--; size>=0; size--) x += v[size];
  return x;}

/* (C) Copr. 1986-92 Numerical Recipes Software >. */

float lbessi0(float x)
{ float ax;
  double y;
  if ((ax = fabs(x))<3.75)
  { y = x/3.75;
    y *= y;
    return my_log(1.0+y*(3.5156229+y*(3.0899424+y*(1.2067492
		  +y*(0.2659732+y*(0.360768e-1+y*0.45813e-2))))));}
  else
  { y = 3.75/ax;
    return (ax-0.5*my_log(ax))+
	   my_log(0.39894228+y*(0.1328592e-1
		  +y*(0.225319e-2+y*(-0.157565e-2+y*(0.916281e-2
		  +y*(-0.2057706e-1+y*(0.2635537e-1+y*(-0.1647633e-1
		  +y*0.392377e-2))))))));}}

float lbessi1(float x)
{ float ax;
  double y;
  assert(x>=0.0);
  if ((ax = fabs(x))<3.75)
  { y = x/3.75;
    y *= y;
    return my_log(ax*(0.5+y*(0.87890594+y*(0.51498869+y*(0.15084934
    		  +y*(0.2658733e-1+y*(0.301532e-2+y*0.32411e-3)))))));}
  else
  { y = 3.75/ax;
    return (ax-0.5*my_log(ax))
	    +my_log(0.39894228+y*(-0.3988024e-1+y*(-0.362018e-2
		    +y*(0.163801e-2+y*(-0.1031555e-1+y*(0.2282967e-1
		    +y*(-0.2895312e-1+y*(0.1787654e-1-y*0.420059e-2))))))));}}

float zriddr(float (*func)(float), float x1, float x2, float xacc)
{ int j;
  float ans, fh, fl, fm, fnew, s, xh, xl, xm, xnew;
  fl = (*func)(x1);
  fh = (*func)(x2);
  if ((fl>0.0&&fh<0.0)||(fl<0.0&&fh>0.0))
  { xl = x1;
    xh = x2;
    ans = UNUSED;
    for (j = 1; j<=MAXIT; j++)
    { xm = 0.5*(xl+xh);
      fm = (*func)(xm);
      s = sqrt(fm*fm-fl*fh);
      if (s==0.0) return ans;
      xnew = xm+(xm-xl)*((fl>=fh?1.0:-1.0)*fm/s);
      if (fabs(xnew-ans)<=xacc) return ans;
      ans = xnew;
      fnew = (*func)(ans);
      if (fnew==0.0) return ans;
      if (SIGN(fm, fnew)!=fm)
      { xl = xm;
	fl = fm;
	xh = ans;
	fh = fnew;}
      else if (SIGN(fl, fnew)!=fl)
      { xh = ans;
	fh = fnew;}
      else if (SIGN(fh, fnew)!=fh)
      { xl = ans;
	fl = fnew;}
      else panic("never get here.");
      if (fabs(xh-xl)<=xacc) return ans;}
    panic("zriddr exceed maximum iterations");}
  else
  { if (fl==0.0) return x1;
    if (fh==0.0) return x2;
    panic("root must be bracketed in zriddr. (l, h) = %f %f", fl, fh);}
  return 0.0;}

void free_c_vector(void *v) {free(v);}

void *_safe_malloc(size_t size, int line, char *file)
{ void *buffer;
  if (!(buffer = malloc(size)))
  { fprintf(stderr, "%s(%d): memory allocation failure. %ld bytes\n",
	    file, line, size);
   exit(1);}
  return buffer;}

Real SQR(Real a) { return a*a; }

/* Tam V'Nishlam Shevah L'El Borei Olam */
