%Aclevsim
%David Honour 2013
%GPL v3
%
%Job control module:
%
-module(job).
-export([test/0]).

-record(emitter, {n, points, ar, excite}).
-record(element, {n, points, ar}).
-record(measure, {n, points}).

-record(job, {freq, c, rho, refl, geoms}).

%   Job id.
%   Job name.
%   Submitting user.
%   Status.
-define(FloatSize, 4).

md5sum(Path) ->
	{ok, [HRtn|_]} = ports:shell_cmd("md5sum " ++ Path),
	FSpace = fun($ ) -> false; (_) -> true end,
	lists:takewhile(FSpace, HRtn).

filesize(Path) ->
	{ok, [HRtn|_]} = ports:shell_cmd("du -b " ++ Path),
	FSpace = fun($	) -> false; (_) -> true end,
	NStr = lists:takewhile(FSpace, HRtn),
	list_to_integer(NStr).

n_points({file, NPoints, Path, Hash}) when is_number(NPoints) ->
	NPoints = filesize(Path) div (3*?FloatSize),
	Hash = md5sum(Path),
	{NPoints, {file, Path, Hash}};
n_points({plane, A, B, Res}) ->
	%Verify A and B are vectors.
	{XRes, YRes} = Res,
	{XRes*YRes, {plane, A, B, Res}}.

check_ar({file, Path, Hash}, N) ->
	N = filesize(Path)/?FloatSize,
	Hash = md5sum(Path),
	ok;
check_ar({const, C}, _) when C > 0 ->
	ok.

check_excite({file, Path, Hash}, N) ->
	N = filesize(Path)/(2*?FloatSize),
	Hash = md5sum(Path),
	ok;
check_excite({const, R, I}, _) when is_number(R) and is_number(I) ->
	ok.

get_geom({emitter, {Points, AR, Excite}}) ->
	{N, Pts} = n_points(Points),
	check_ar(AR, N),
	check_excite(Excite, N),
	#emitter{n=N, points=Pts, ar=AR, excite=Excite};
get_geom({element, {Points, AR}}) ->
	{N, Pts} = n_points(Points),
	check_ar(AR, N),
	#element{n=N, points=Pts, ar=AR};
get_geom({measure, {Points}}) ->
	{N, Pts} = n_points(Points),
	#measure{n=N, points=Pts}.

create_job(Freq, C, Rho, Refl, Geoms) when is_number(Freq) and is_number(C) and is_number(Rho) and is_integer(Refl) and is_list(Geoms) ->
	#job{freq=Freq, c=C, rho=Rho, refl=Refl, geoms=Geoms}.

test() ->
	Hash = md5sum("testfile"),
	{measure, 2, {file, "testfile", Hash}} = get_geom({measure, {{file, 2, "testfile", Hash}}}).
