typedef struct {
  int len;
  float *f;
} Cdyn;

Cdyn *Cdyn_create(int);
void Cdyn_destroy(Cdyn *);
void Cdyn_print(Cdyn *);
void Cdyn_add(Cdyn *, float *);

void *Fdyn_create(int);
void Fdyn_print(void *);
void Fdyn_destroy(void *);
