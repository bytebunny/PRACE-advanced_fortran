#include <stdlib.h>
#include "c_libcall.h"

/* the void argument params is only handed through */


float Sum_fun(float (*fun)(float x, void *params), void *params) {
    int i;
    float sum=0.0;
    float x=0.0;
    for (i=0; i<10; i++) {
	sum += fun(x, params);
	x += 1.0;
    }
    return sum;
}
