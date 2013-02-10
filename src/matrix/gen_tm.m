function tm = gen_tm(a, b, k)
	#Generate a transfer matrix a -> b
	tm = i*ones(size(b)(1), size(a)(1));
	for l = 1:size(b)(1)
		bv = b(l,1:3);
		for m = 1:size(a)(1)
			r = norm(a(m,1:3) - bv);
			tm(l,m) = a(m,4) * e^(-i*k*r) / r;
		end
	end
endfunction
