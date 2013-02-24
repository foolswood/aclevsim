-module(ports).

-export([shell_cmd/1]).

cmd_loop(P, Output) ->
	receive
		{P, {data, Data}} ->
			cmd_loop(P, [Data | Output]);
		{P, {exit_status, 0}} ->
			{ok, Output};
		{P, {exit_status, Status}} ->
			{error, {Status, Output}};
		{'EXIT', P, Reason} ->
			{error, Reason}
	end.

shell_cmd(Cmd) ->
	try open_port({spawn, Cmd}, [exit_status, stderr_to_stdout]) of
		P -> cmd_loop(P, [])
	catch
		_:Reason -> {error, Reason}
	end.
