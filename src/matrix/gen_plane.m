function p = gen_plane(pos, xv, yv, detail)
	#Generate plane geometries
	a = (norm(xv)/detail(1))*(norm(yv)/detail(2));
	xv = linspace([0,0,0], xv, detail(1))';
	yv = linspace([0,0,0], yv, detail(2))';
	odox = ones(detail(1),1);
	odoy = ones(detail(2),1);
	p(:, 1:3) = kron(kron(pos, odox) + xv, odoy) + kron(odox, yv);
	p(:,4) = a*ones(1,detail(1)*detail(2));
endfunction
