#include <stdlib.h>
#include <gsl/gsl_math.h>

gsl_function *fgsl_function_cinit(double (*func)(double x, void *params), void *params) {
    gsl_function *result;
    result = (gsl_function *) malloc(sizeof(gsl_function));
    result->function = func;
    result->params = params;
    return result;
}

void fgsl_function_cfree(gsl_function *fun) {
    free(fun);
}

