-module(job_inst).

%-export([add/2, hello/0, greet_and_add_two/1]).

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

%gen_tm(A, B)

%stitch_tm(A, B)

%composite_tm(A, B, MORE_STUFF)

%optimise(Instructions)

geom_load(G) ->
	#load{path=G}.

new_in() ->
	{orddict:new(), digraph:new([acyclic])}.

deps_satisfied(_, []) ->
	true;
deps_satisfied(G, [Out|Tail]) ->
	{Out, In} = digraph:vertex(G, Out),
	case In#in.status of
		complete ->
			deps_satisfied(G, Tail);
		_ ->
			false
	end.

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
			ordsets:fold(fun ?MODULE:get_ins/2, {Ins, U, Acc}, Unstarted)
	end.

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

paths_to_in(Paths) ->
	{Insts, TmIns} = lists:foldl(fun ?MODULE:path_to_tm_instructions/2, {new_in(), orddict:new()}, Paths),
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
	I = paths_to_in(P),
	{_, {_, Flooble}} = I,
	InPrint = fun(In, Acc) ->
		[digraph:vertex(Flooble, In) | Acc]
	end,
	%{Ins, Flooble} = get_ins(I),
	%L = lists:foldl(InPrint, [], Ins),
	Completed = fun(V, Acc) ->
		[update_in(Flooble, V, complete) | Acc]
	end,
	%L2 = lists:foldl(InPrint, L, lists:foldl(Completed, [], Ins)),
	%{Ins2, Flooble} = get_ins(I),
	%lists:foldl(InPrint, [], Ins2).
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
