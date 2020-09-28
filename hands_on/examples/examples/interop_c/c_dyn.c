#include <stdlib.h> 
#include "c_dyn.h"

Cdyn *Cdyn_create(int len) {
  // establish memory for object and dynamic component
  Cdyn *this = (Cdyn *) malloc(sizeof(Cdyn));
  int i;
  this->len = len;
  this->f = (float *) malloc(len*sizeof(float));
  for (i=0; i<len; i++) {
    this->f[i] = (float) i+1;
  }
  return this;
}

void Cdyn_destroy(Cdyn *v) {
  free(v->f);
  free(v);
}

void Cdyn_add(Cdyn *v, float *a) {
  // a must contain at least v->len elements.
  int i;
  for (i=0;i<v->len;i++) {
    v->f[i] = v->f[i] + a[i];
  }
}
