%Aclevsim
%David Honour 2013
%GPL v3
%
%Job control module:
%
-module(geom).
-export([test/0, n_points/1, emitters/1, elements/1, measures/1]).

-record(emitter, {n, points, ar, excite}).
-record(element, {n, points, ar}).
-record(measure, {n, points}).

md5sum(Path) ->
	{ok, [HRtn|_]} = ports:shell_cmd("md5sum " ++ Path),
	FSpace = fun($ ) -> false; (_) -> true end,
	lists:takewhile(FSpace, HRtn).

filesize(Path) ->
	{ok, [HRtn|_]} = ports:shell_cmd("du -b " ++ Path),
	FSpace = fun($	) -> false; (_) -> true end,
	NStr = lists:takewhile(FSpace, HRtn),
	list_to_integer(NStr).

check_geom({file, NPoints, Path, Hash}) when is_number(NPoints) ->
	Size = matrix:mem(NPoints, 3, real),
	Size = filesize(Path),
	Hash = md5sum(Path),
	{NPoints, {file, Path, Hash}};
check_geom({plane, A, B, Res}) ->
	%Verify A and B are vectors.
	{XRes, YRes} = Res,
	{XRes*YRes, {plane, A, B, Res}}.

check_ar({file, Path, Hash}, N) ->
	Size = matrix:mem(N, 1, real),
	Size = filesize(Path),
	Hash = md5sum(Path),
	ok;
check_ar({const, C}, _) when C > 0 ->
	ok.

check_excite({file, Path, Hash}, N) ->
	Size = matrix:mem(N, 1, complex),
	Size = filesize(Path),
	Hash = md5sum(Path),
	ok;
check_excite({const, R, I}, _) when is_number(R) and is_number(I) ->
	ok.

get_geom({emitter, {Points, AR, Excite}}) ->
	{N, Pts} = check_geom(Points),
	check_ar(AR, N),
	check_excite(Excite, N),
	#emitter{n=N, points=Pts, ar=AR, excite=Excite};
get_geom({element, {Points, AR}}) ->
	{N, Pts} = check_geom(Points),
	check_ar(AR, N),
	#element{n=N, points=Pts, ar=AR};
get_geom({measure, {Points}}) ->
	{N, Pts} = check_geom(Points),
	#measure{n=N, points=Pts}.

n_points(Geom) when is_record(Geom, emitter) or is_record(Geom, element) or is_record(Geom, measure) ->
	element(2, Geom).

emitters(Geoms) ->
	F = fun(E) ->
		is_record(E, emitter)
	end,
	lists:filter(F, Geoms).

elements(Geoms) ->
	F = fun(E) ->
		is_record(E, element)
	end,
	lists:filter(F, Geoms).

measures(Geoms) ->
	F = fun(E) ->
		is_record(E, measure)
	end,
	lists:filter(F, Geoms).

test() ->
	Hash = md5sum("testfile"),
	M = get_geom({measure, {{file, 2, "testfile", Hash}}}),
	{measure, 2, {file, "testfile", Hash}} = M,
	2 = n_points(M).
