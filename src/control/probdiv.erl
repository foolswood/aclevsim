-module(probdiv).

-compile(export_all).
%-export([add/2, hello/0, greet_and_add_two/1]).

%Aclevsim
%David Honour 2013
%GPL v3
%
%Problem division module.
%  Divide job into tasks.
%  Estimate task times.

tm_mem(NA, NB) ->
	Complexsize * NA * NB.

tm_time(NA, NB) ->
	Bmarktime * NA * NB.

gross_tasks(Job) ->
	.
