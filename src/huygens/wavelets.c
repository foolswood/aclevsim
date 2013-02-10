typedef struct wavelet {
	//amplitude, angular freq, phase, location
	float A, w, p;
	vector loc;
} wavelet;

typedef struct prob {
	unsigned int nwls;
	wavelet *wls;
	float k;
} prob;

inline float pressure(const prob const *s, const vector const loc, const float t) {
	//Reports the pressure at the specified point and time
	float r, P = 0;
	vector v = Vmk();
	const wavelet *wl;
	unsigned i;
	for (i=0; i < s->nwls; i++) {
		wl = (s->wls)+i;
		Vsub(v, loc, wl->loc);
		r = Vmod(v);
		P += wl->A*cos(wl->w*t - (s->k)*r + wl->p)/r;
	}
	free(v);
	return P;
}
