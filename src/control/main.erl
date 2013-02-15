-module(main).

-compile(export_all).
%-export([add/2, hello/0, greet_and_add_two/1]).

pants(X) when X > 5, X < 10, X rem 2 == 0 -> true;
pants(_) -> false.

zip([],[]) -> [];
zip([X|Xs],[Y|Ys]) -> [{X,Y}|zip(Xs,Ys)].

tzip(A,B) -> tzip(A,B,[]).
tzip([],[],Acc) -> lists:reverse(Acc);
tzip([X|Xs],[Y|Ys],Acc) -> tzip(Xs,Ys,[{X,Y}|Acc]).

lzip([],_) -> [];
lzip(_,[]) -> [];
lzip([X|Xs],[Y|Ys]) -> [{X,Y}|lzip(Xs,Ys)].

ltzip(A,B) -> ltzip(A,B,[]).
ltzip([],_,Acc) -> lists:reverse(Acc);
ltzip(_,[],Acc) -> lists:reverse(Acc);
ltzip([X|Xs],[Y|Ys],Acc) -> ltzip(Xs,Ys,[{X,Y}|Acc]).

%Aclevsim
%David Honour 2013
%GPL v3
%
%Main module.
%
%Job control:
% Add job.
%  Analyse job.
%   Detect method.
%  Divide job into tasks (using method specific module).
%  Estimate task times (method specific).
%  Add to job list.
% List jobs and their status.
%  Job filtering:
%   By job id.
%   By submitting user.
%  Info filtering:
%   Job id.
%   Job name.
%   Submitting user.
%   Status.
% Remove job (by job id).
%  Remove job from job list.
%  Stop active tasks.
%  Remove job files.
% Job priority.
%  Set low/medium/high.
%
%Task control:
% Update tasks (from jobs).
% Current task finished.
%  Start next task or sleep.
%
%JOB CONTROL
%job list is an ordict
