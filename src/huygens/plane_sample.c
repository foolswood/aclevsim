void sample(const vector const scanstart, const vector const xoutput, const vector const youtput, const unsigned xsamples, const unsigned ysamples, const prob const *s, float **grid) {
	#if (DIMENSIONS == 2 || DIMENSIONS == 3)
	//Determine axis samples
	unsigned i, j;
	vector pos, xdelta, ydelta;
	pos = Vmk();
	xdelta = Vmk();
	ydelta = Vmk();
	//Create Deltas
	Vtimes_scalar(xdelta, xoutput, 1.0/xsamples);
	Vtimes_scalar(ydelta, youtput, 1.0/ysamples);
	//scan y delta
	for (i = 0; i < ysamples; i++) {
		Vzero(pos);
		Vtimes_scalar(pos, xdelta, i);
		Vadd(pos, pos, scanstart);
		//scan x delta
		for (j = 0; j < xsamples; j++) {
			grid[j][i] = pow(pressure(s, pos, 0), 2); //THIS IS JUST FOR t=0 VERY UNGENERAL
			grid[j][i] += pow(pressure(s, pos, M_PI/4), 2); //THIS IS JUST FOR t=0 VERY UNGENERAL
			grid[j][i] += pow(pressure(s, pos, M_PI/2), 2); //THIS IS JUST FOR t=0 VERY UNGENERAL
			grid[j][i] += pow(pressure(s, pos, (3*M_PI)/4), 2); //THIS IS JUST FOR t=0 VERY UNGENERAL
			Vadd(pos, pos, ydelta);
		}
	}
	free(pos);
	free(xdelta);
	free(ydelta);
	#else
	#error "2 or 3 Dimensions Only"
	#endif
}

void gen_gp_mat(const float const **data, const unsigned xsamples, const unsigned ysamples, const char *fpath) {
	//produces a file for gnuplot in matrix format
	unsigned i, j;
	FILE *f = fopen(fpath, "w");
	for (i = 0; i < ysamples; i++) {
		for (j = 0; j < xsamples - 1; j++) {
			fprintf(f, "%f	", data[i][j]);
		}
		fprintf(f, "%f\n", data[i][j]);
	}
}
