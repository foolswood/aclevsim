-module(main).

-export([test/0]).

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

test() ->
	%Load the file
	{ok, TestFile} = file:consult("test.sim"),
	{_, Freq} = lists:keyfind(freq, 1, TestFile),
	{_, C} = lists:keyfind(c, 1, TestFile),
	{_, Rho} = lists:keyfind(rho, 1, TestFile),
	{_, Refl} = lists:keyfind(refl, 1, TestFile),
	{_, TestGeoms} = lists:keyfind(geom, 1, TestFile),
	%Convert to geoms
	TG2G = fun(TG, Acc) ->
		[geom:get_geom(TG) | Acc]
	end,
	Geoms = lists:foldl(TG2G, [], TestGeoms),
	%Create job
	Job = job:create_job(Freq, C, Rho, Refl, Geoms),
	Ins = job_inst:job_to_ins(Job),
	{Is, Dg} = job_inst:get_ins(Ins),
	DOI =  fun(I) ->
		job_inst:do_in(Job, I, Dg)
	end,
	lists:map(DOI, Is),
	{Is2, Dg} = job_inst:get_ins(Ins),
	io:format("~n~n Round 2 ~n~n"),
	lists:map(DOI, Is2).
