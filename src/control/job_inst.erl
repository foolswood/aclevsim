-module(job_inst).

-export([test/0, job_to_ins/1, get_ins/1, do_in/2]).

%Aclevsim
%David Honour 2013
%GPL v3
%
%Problem division module.
%  Divide job into tasks.
%  Estimate task times.

-record(tm, {size}).
-record(p, {p}).
-record(load, {path, n=1, m=1}).
-record(in, {status, instr}).

geom_load(G) ->
	#load{path=G}.

new_ins() ->
	{orddict:new(), digraph:new([acyclic])}.

%deps_satisfied(_, []) ->
%	true;
%deps_satisfied(G, [Out|Tail]) ->
%	{Out, In} = digraph:vertex(G, Out),
%	case In#in.status of
%		complete ->
%			deps_satisfied(G, Tail);
%		_ ->
%			false
%	end.

update_in(Dg, V, Status) ->
	{V, Prev} = digraph:vertex(Dg, V),
	digraph:add_vertex(Dg, V, Prev#in{status=Status}).

get_ins({Start, {_, Ins}}) ->
	%get an not done instruction (if possible)
	U = fun (V) ->
		{V, L} = digraph:vertex(Ins, V),
		case L#in.status of
			unstarted ->
				true;
			_ ->
				false
		end
	end,
	case U(Start) of
		true ->
			{Ins, U, O} = get_ins(Start, {Ins, U, ordsets:new()}),
			{ordsets:to_list(O), Ins};
		false ->
			{[], Ins}
	end.
	
get_ins(V, {Ins, U, Acc}) ->
	Outs = digraph:out_neighbours(Ins, V),
	Unstarted = ordsets:filter(U, Outs),
	case ordsets:size(Unstarted) of
		0 ->
			{Ins, U, ordsets:add_element(V, Acc)};
		_ ->
			ordsets:fold(fun get_ins/2, {Ins, U, Acc}, Unstarted)
	end.

job_to_ins(Job) ->
	R = job:refl(Job),
	Geoms = job:geoms(Job),
	Emitters = geom:emitters(Geoms),
	Elements = geom:elements(Geoms) ++ Emitters,
	Measures = geom:measures(Geoms),
	For_Measure = fun (Measure, {Results, Ins}) ->
		For_Emitter = fun (Emitter, PathAcc) ->
			PathAcc ++ gen_paths(Emitter, Measure, Elements, R)
		end,
		Paths = lists:foldl(For_Emitter, [], Emitters),
		{Result, NewIns} = paths_to_in(Paths, Ins),
		{[Result | Results], NewIns}
	end,
	{Results, Ins} = lists:foldl(For_Measure, {[], new_ins()}, Measures),
	add_in(ok, Results, Ins).

add_in(In, Deps, {Od, Dg}) ->
	case orddict:find({In, Deps}, Od) of
		{ok, Value} ->
			{Value, {Od, Dg}};
		error ->
			V = digraph:add_vertex(Dg, orddict:size(Od), #in{instr=In, status=unstarted}),
			NOd = orddict:store({In, Deps}, V, Od),
			F = fun(Dep, _) ->
				digraph:add_edge(Dg, V, Dep)
			end,
			lists:foldl(F, ok, Deps),
			{V, {NOd, Dg}}
	end.

paths_to_in(Paths, Existing) ->
	{Insts, TmIns} = lists:foldl(fun path_to_tm_instructions/2, {Existing, orddict:new()}, Paths),
	G = fun(Path, {Acc, Ins}) ->
		P = add_in(#p{p="Piss"}, [], Ins),
		{Result, NewIns} = path_to_p_ins(Path, TmIns, P),
		{[Result | Acc], NewIns}
	end,
	{ToSum, InstsP} = lists:foldl(G, {[], Insts}, Paths),
	add_in(sum, ToSum, InstsP).
	
path_to_p_ins([A, B], Tms, {P, Ins}) ->
	Tm = orddict:fetch([A, B], Tms),
	add_in(p, [Tm, P], Ins);
path_to_p_ins([A, B | Tail], Tms, Acc) ->
	path_to_p_ins([B | Tail], Tms, path_to_p_ins([A, B], Tms, Acc)).

path_to_tm_instructions([A, B], {Ins, Acc}) ->
	Step = [A, B],
	case orddict:find(Step, Acc) of
		{ok, _} ->
			{Ins, Acc};
		error ->
			{AI, InsA} = add_in(geom_load(A), [], Ins),
			{BI, InsB} = add_in(geom_load(B), [], InsA),
			{In, NewIns} = add_in(#tm{size={A,B}}, [AI, BI], InsB),
			{NewIns, orddict:store(Step, In, Acc)}
	end;
path_to_tm_instructions([A, B | Tail], Acc) ->
	NewAcc = path_to_tm_instructions([A, B], Acc),
	path_to_tm_instructions([B | Tail], NewAcc).

%Warning resulting list length = (Elements-1)^((1+R)*(R/2))
gen_paths(Start, End, Elements, Reflections) ->
	Step = fun(Base, Acc) -> %Add a step to the path Base and append to Acc
		[Head | _] = Base,
		[[A | Base] || A <- Elements -- [Head]] ++ Acc
	end,
	Paths = gp(Step, Reflections, [[Start]], [[Start]]),
	Glue = fun(Path) -> [End | Path] end, %Glue the End on
	FullPaths = lists:map(Glue, Paths),
	lists:map(fun lists:reverse/1, FullPaths). %Since paths built in reverse order

gp(_, 0, _, Acc2) ->
	Acc2;
gp(StepFun, Reflections, Acc, Acc2) ->
	A = lists:foldl(StepFun, [], Acc),
	A2 = Acc2 ++ A,
	gp(StepFun, Reflections - 1, A, A2).

test() ->
	P = gen_paths(1, 3, [1,2], 1),
	I = paths_to_in(P, new_ins()),
	{_, {_, Flooble}} = I,
	InPrint = fun(In, Acc) ->
		[digraph:vertex(Flooble, In) | Acc]
	end,
	Completed = fun(V, Acc) ->
		[update_in(Flooble, V, complete) | Acc]
	end,
	{P, lists:reverse(test(I, InPrint, Completed, []))}.
test(I, InPrint, Comp, Acc) ->
	case get_ins(I) of
		{[], _} ->
			[done | Acc];
		{Ins, _} ->
			lists:foldl(Comp, [], Ins),
			NewAcc = lists:foldl(InPrint, Acc, Ins),
			test(I, InPrint, Comp, [then | NewAcc])
	end.

%Functions for actually doing the instructions

gen_tm(A, B, Freq, Rho, C) ->
	matrix:size(geom:size(A), geom:size(B), complex).

%stitch_tm(A, B)

%composite_tm(A, B, MORE_STUFF)

do_in(Job, In) ->
	gen_tm(ok, ok, ok, ok, ok).
