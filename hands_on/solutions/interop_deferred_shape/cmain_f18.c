#include <stdio.h>
#include <stdlib.h>
#include <ISO_Fortran_binding.h>

void generate_data(CFI_cdesc_t *);

int main() {
    int i, il;
    CFI_cdesc_t *array = 
	(CFI_cdesc_t *) malloc(sizeof(CFI_CDESC_T(1))); 
      
    CFI_establish(  array, NULL, CFI_attribute_allocatable, 
		    CFI_type_float, sizeof(float), 1, NULL );

    generate_data(array);

    il = array->dim[0].lower_bound;
    printf("Lower bound is %i\n",il);
    for (i=il; i<il+array->dim[0].extent; i++) {
	CFI_index_t subscripts[1] = { i };
        printf("Element %i has value %10.3f\n",i,
	       *( (float *) CFI_address(array, subscripts)));       
    }
    CFI_deallocate(array);  // Fortran deallocation
    free(array);            // remove C-allocated memory for descriptor
}
