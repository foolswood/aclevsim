-module(node_caps).
-export([test/0]).

-record(node, {mem, cpu}).

get_local() ->
	{ok, Conf0} = file:consult("system.conf"), %Some kind of confdir setting?
	{value, {mem, Mem}, Conf1} = lists:keytake(mem, 1, Conf0),
	{value, {cpu, Cpu}, Conf2} = lists:keytake(cpu, 1, Conf1),
	[] = Conf2,
	#node{mem=Mem, cpu=Cpu}.

mem(Node) ->
	Node#node.mem.

test() ->
	N = get_local(),
	1024 = mem(N),
	ok.
