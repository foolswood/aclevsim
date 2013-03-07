%Aclevsim
%David Honour 2013
%GPL v3
%
%Matrix utils module:
%
-module(matrix).
-export([mem/3]).

-define(FloatSize, 4).

mem(A, B, real) ->
	A*B*?FloatSize;
mem(A, B, complex) ->
	A*B*2*?FloatSize.
