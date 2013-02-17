tic() #Start timing

emit = gen_plane([-0.25, -0.25, 1.5], [0.5, 0, 0], [0, 0.5, 0], 20);
d = 30;
meas = gen_plane([-1, 0, -1], [2, 0, 0], [0, 0, 2], d);

k = 30;
#Generate the transfer matrix emitter -> measurement plane
Tem = gen_tm(emit, meas, k);

#Amplitude of area elements (set to 1)
U = ones(size(emit)(1), 1);

#Generate pressures
P = Tem * U;

toc() #Stop timing

plot_alf(meas, P, d);
