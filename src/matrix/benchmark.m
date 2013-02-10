function benchmark(md, ed, rd)

tic() #Start timing

emit = gen_plane([-0.25, -0.25, 1.5], [0.5, 0, 0], [0, 0.5, 0], ed);
meas = gen_plane([-1, 0, -1], [2, 0, 0], [0, 0, 2], md);
refl = gen_plane([-1, -1, -1.5], [2, 0, 0], [0, 2, 0], rd);

omega = 10000;
c = 343.2;
rho = 1.225;
A = 1;

k = omega/c;
lambda = 2*pi*c/omega;

#Generate the transfer matrix emitter -> measurement plane
Tem = gen_tm(emit, meas, k);
Trm = gen_tm(refl, meas, k);
Ter = gen_tm(emit, refl, k);
Tre = gen_tm(refl, emit, k);

#Amplitude of area elements (set to 1)
U = A * (omega*rho*c/lambda) * ones(size(emit)(1), 1);

#Generate pressures (subsequent lines are next reflection)
P = Tem * U;
P += (i/lambda) * Trm * Ter * U;
P += (i/lambda)^2 * Tem * Tre * Ter * U;
P += (i/lambda)^3 * Trm * Ter * Tre * Ter * U;

toc() #Stop timing

whos() #Memory

#plot_alf(meas, P, md);

endfunction

#ACTUAL BENCHMARKING BIT

for s = 61:2:100
	benchmark(s, 10, 10);
end
