#include <stdlib.h>
#include <stdio.h>
#include "c_libcall.h"

/* Client program   */

typedef struct {
    int scale;
    float offset;
} My_Stuff;

float My_fun(float x, void *params) {
    My_Stuff *Obj_Stuff = (My_Stuff *) params;
    float My_fun_result = (float) Obj_Stuff->scale * x + Obj_Stuff->offset;
    return My_fun_result;
}

int main() {
    My_Stuff params = { 2, 3.5 };
    double result;
    result = Sum_fun(&My_fun, (void *) &params);
    printf("Result of calculation is: %13.7f\n",result);
}
