#include "gsl/gsl_sf.h"

#define IDIM 11

int main() {
    gsl_sf_result r[IDIM];
    double x[] = { 0.0, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 1.8, 2.0 };
    int i, istat;

    printf("  x       erf(x)      Error \n");
    for (i=0; i<IDIM; i++) {
	gsl_sf_erf_e(x[i], &r[i]);
	printf("%5.2f %12.5e %12.5e\n", x[i], r[i].val, r[i].err);
    }
}
