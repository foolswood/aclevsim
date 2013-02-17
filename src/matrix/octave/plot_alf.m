function plot_alf(meas, P, d)
	#Reshape measurement positions
	xx = reshape(meas(:,1), d, d);
	yy = reshape(meas(:,3), d, d);
	#reshape (and square) pressure
	P = real(reshape(P.^2, d, d));
	#Plot contours
	contourf(xx, yy, P);
endfunction
