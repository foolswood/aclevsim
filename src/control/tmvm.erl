-module(tmvm).

-export([vm_instance/0, geom/1, tm/2, u/0, done/0, p/0, do_in/2]).

geom(G) ->
	ok.

tm(A, B) ->
	ok.

u() ->
	ok.

p() ->
	ok.

done() ->
	ok.

do_in(Job, In) ->
	ok.

%BELOW IGNORE

get_output(P) ->
	io:format("awaiting~n"),
	receive
		{P, {data, {eol, Data}}} ->
			{ok, string:strip(Data)};
		{P, {exit_status, 0}} ->
			terminate;
		{P, {exit_status, Status}} ->
			{error, Status};
		{'EXIT', P, Reason} ->
			{error, Reason};
		{P, Other} ->
			Other
	end.

get_vm_opts(VM) ->
	Rcol = fun(Line) ->
		I = string:rchr(Line, $ ),
		string:substr(Line, I+1)
	end,
	{ok, Opt1} = get_output(VM),
	{ok, Opt2} = get_output(VM),
	A = string:to_integer(Rcol(Opt1)),
	B = string:to_integer(Rcol(Opt2)),
	{A, B}.

vm_instance() ->
	try open_port({spawn, "./print2lines.py"}, [exit_status, stderr_to_stdout, {line, 256}]) of
		P -> get_vm_opts(P), port_command(P, "andy\n"), io:format("messaged~n"), get_output(P)
	catch
		_:Reason -> {error, Reason}
	end.

%echo(P, Msg) ->
%	ValidMsg1 = case is_newline_terminated(Msg) of
%		true  -> Msg;
%		false -> erlang:error(badarg)
%	end,
%	io:format("pass 1~n"),
%	ValidMsg2 = case count_chars(ValidMsg1, $\n) of
%		1     -> ValidMsg1;
%		_     -> erlang:error(badarg)
%	end,
%	port_command(P, ValidMsg2).

%count_chars(String, Char) ->
%	length([X || X <- String, X == Char]).

%is_newline_terminated([])    -> false;
%is_newline_terminated([$\n]) -> true;
%is_newline_terminated([_|T]) -> is_newline_terminated(T).
