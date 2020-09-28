#include <stdio.h>
#include <stdlib.h>

#include "deferred.h"

int main() {
    Handle h;
    int i;
    Generate_data(&h);
    if (h.real) {
	for (i=0; i<h.size; i++) {
	    printf("Element %i has value %10.3f\n",i+1,h.real[i]);
	}
    }
}

