omega = 5.25e6*2*pi;
c = 1470.0;
rho = 998.94;
A = 0.0001; #TOTAL GUESS

k = omega/c;
dtheta = 0.00;
lambda = 2*pi*c/omega;
0
emit_a = gen_plane([-0.0075, -0.0075, 0.001], [0, 0.015, 0], [0, 0, -0.002], [150, 20]);
1
emit_b = gen_plane([0.0075*(1+sin(dtheta)), -0.0075*cos(dtheta), 0.001], [0.015*sin(dtheta), 0.015*cos(dtheta), 0], [0, 0, -0.002], [150, 20]);
2
meas = gen_plane([-0.0005, -0.0005, 0], [0.0005, 0, 0], [0, 0.0005, 0], [50, 50]);
3

#Generate the transfer matrix emitter -> measurement plane
Tam = gen_tm(emit_a, meas, k);
#load sonotweezers/perfect-alignments/2010-100x60-Cr0.24-R3-notbfb-Txm+P.mat Tam
4
Tbm = gen_tm(emit_b, meas, k);
5
Tab = gen_tm(emit_a, emit_b, k);
#load 2010-notbfb-Tab.mat Tab;
6
#Tba = gen_tm(emit_b, emit_a, k);
Tba = Tab.';
7

#Amplitude of area elements
U = A * (omega*rho*c/lambda) * ones(size(emit_a)(1), 1);
8

#Generate pressures (subsequent lines are next reflection)
P = Tam * U;
Cr = 0.24;
9
P += (Cr*i/lambda) * Tbm * Tab * U;
10
P += (Cr*i/lambda)^2 * Tam * Tba * Tab * U;
11
P += (Cr*i/lambda)^3 * Tbm * Tab * Tba * Tab * U;
12
P += Tbm * U;
13
P += (Cr*i/lambda) * Tam * Tba * U;
14
P += (Cr*i/lambda)^2 * Tbm * Tab * Tba * U;
15
P += (Cr*i/lambda)^3 * Tam * Tba * Tab * Tba * U;
16

#plot_alf(meas, P, md);
