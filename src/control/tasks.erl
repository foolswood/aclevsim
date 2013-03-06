%Aclevsim
%David Honour 2013
%GPL v3
-module(tasks).
-compile(export_all).
%-export([add/2, hello/0, greet_and_add_two/1]).

%Register node capabilities
start(JC) ->

%Task control:
% Launch task:
%   poll until task available
% Task complete:
%  Report completion.
%  Get next.
% Task failed:
%  Gather info.
%  Report failure.
%  Get next.


