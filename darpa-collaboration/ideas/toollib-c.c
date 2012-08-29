/* LaHaShem HaAretz U'Mloah */

#include "toollib-c.h"
/* Global Variables */

struct xvimage *vect = NULL;

/* Function Definitions */

/* C int vectors and matrices */

int *make_c_int_vector(int l)
{ int *v;
  v = (int *)malloc(l*sizeof(int));
  if (v==NULL) panic("make_c_int_vector: out of memory.  size = %d", l);
  return v;}

int c_int_vector_ref(int *v, int x) {return v[x];}

void c_int_vector_set(int *v, int x, int i) {v[x] = i;}

void free_c_int_vector(int *v) {free(v);}

int **make_c_int_matrix(int h, int w)
{ int **m, i;
  m = (int **)malloc(h*sizeof(int *));
  if (m==NULL) panic("make_c_int_matrix: out of memory.  size = %d %d", h, w);
  for (i = 0; i<h; i++)
  { m[i] = (int *)malloc(w*sizeof(int));
    if (m[i]==NULL)
    { panic("make_c_int_matrix: out of memory.  size = %d %d", h, w);}}
  return m;}

int c_int_matrix_ref(int **m, int y, int x) {return m[y][x];}

void c_int_matrix_set(int **m, int y, int x, int i) {m[y][x] = i;}

void free_c_int_matrix(int **m, int h)
{ int i;
  for (i = 0; i<h; i++) free(m[i]);
  free(m);}

/* Interruptible usleep */

int wall_clock(void)
{ struct tms buf;
  return times(&buf);}

/* Tam V'Nishlam Shevah L'El Borei Olam */
