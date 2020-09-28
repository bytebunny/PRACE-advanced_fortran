#include "c_dyn.h"

#define LEN 5

float a[5] = {-1.,1.,3.,5.,7.};

int main() {
  Cdyn *h;
  void *fhandle;

  h = Cdyn_create(LEN);
  Cdyn_add(h, a);
  Cdyn_print(h);
  Cdyn_destroy(h);

  fhandle = Fdyn_create(LEN);
  Fdyn_print(fhandle);
  Fdyn_destroy(fhandle);
}
