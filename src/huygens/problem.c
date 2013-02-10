#define DIMENSIONS 3

#include "vectors.c"
#include "wavelets.c"
#include "plane_sample.c"

vector *gen_units(float angles[DIMENSIONS-1]) {
	vector *units = calloc(sizeof(vector*), 2);
	units[0] = Vmk();
	units[1] = Vmk();
	#if DIMENSIONS == 2
	#error "Not currently implemented"
	#elif DIMENSIONS == 3
	units[0][0] = cos(angles[0]);
	units[0][1] = 0;
	units[0][2] = sin(angles[0]);
	units[1][0] = 0;
	units[1][1] = cos(angles[1]);
	units[1][2] = sin(angles[1]);
	#endif
	return units;
}

void ring_emitter(wavelet *wls, float A, float w, float p, vector loc, vector *units, float radius, unsigned points) {
	//currently doesn't do 2d
	int i;
	vector va, vb;
	float sweep = (2*M_PI)/points;
	va = Vmk();
	vb = Vmk();
	for (i=0; i<points; i++) {
		Vtimes_scalar(va, units[0], cos(sweep*i));
		Vtimes_scalar(vb, units[1], sin(sweep*i));
		Vadd(va, va, vb);
		Vadd(va, va, loc);
		wls[i].A = A;
		wls[i].w = w;
		wls[i].p = p;
		wls[i].loc = Vheap(va);
	}
	free(va);
	free(vb);
}

unsigned circ_emitter(wavelet *wls, float A, float w, float p, vector loc, float angles[DIMENSIONS-1], float radius, unsigned points, unsigned rings) {
	vector *units = gen_units(angles);
	//rings
	unsigned r, s, t;
	float pr = rings;
	t = 0;
	radius /= rings;
	pr = (1+pr)*(pr/2);
	pr = (points-1)/pr;
	for (r=1; r<(rings+1); r++) {
		s = round(r*pr);
		ring_emitter(wls+t, A, w, p, loc, units, r*radius, s);
		t += s;
	}
	//center point
	if (t - points) {
		wls[t].A = A;
		wls[t].p = p;
		wls[t].w = w;
		wls[t].loc = Vheap(loc);
		t++;
	}
	free(units);
	return t;
}

float **heap_array(const unsigned x, const unsigned y) {
	unsigned i;
	float **array = calloc(y, sizeof(float*));
	for (i=0; i<y; i++) {
		array[i] = calloc(x, sizeof(float));
	}
	return array;
}

void free_heap_array(float **ha, const unsigned y) {
	unsigned i;
	for (i=0; i<y; i++) {
		free(ha[i]);
	}
	free(ha);
}

int main(void) {
	const unsigned n_wls = 500;
	vector loc, va, vb;
	unsigned width = 400, height = 400;
	loc = Vmk();
	va = Vmk();
	vb = Vmk();
	float angles[2];
	wavelet wls[n_wls];
	float **data = heap_array(width, height);
	prob s = {n_wls, wls, 20};
	//make emitters
	angles[0] = -0.25;
	angles[1] = 0.01;
	loc[0] = -8;
	loc[1] = 0;
	loc[2] = 0;
	circ_emitter(wls, 1, 1, 0, loc, angles, 5, n_wls/2, n_wls/20);
	angles[0] = 0.25;
	angles[1] = -0.01;
	loc[0] = 8;
	loc[1] = 0;
	loc[2] = 0;
	circ_emitter(wls+(n_wls/2), 1, 1, 0, loc, angles, 5, n_wls/2, n_wls/20);
	//take samples
	loc[0] = -10;
	loc[1] = 8;
	loc[2] = 5;
	va[0] = 20;
	va[1] = 0;
	va[2] = 0;
	vb[0] = 0;
	vb[1] = 0;
	vb[2] = 40;
	sample(loc, va, vb, width, height, &s, data);
	gen_gp_mat((const float**) data, width, height, "output.dat");
	free(loc);
	free(va);
	free(vb);
	free_heap_array(data, height);
	return 0;
}
