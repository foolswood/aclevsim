%Aclevsim
%David Honour 2013
%GPL v3
%
%Job control module:
%
-module(job).
-export([test/0, create_job/5, params/1, geoms/1, refl/1]).

-record(job, {freq, c, rho, refl, geoms}).

%   Job id.
%   Job name.
%   Submitting user.
%   Status.

create_job(Freq, C, Rho, Refl, Geoms) when is_number(Freq) and is_number(C) and is_number(Rho) and is_integer(Refl) and is_list(Geoms) ->
	#job{freq=Freq, c=C, rho=Rho, refl=Refl, geoms=Geoms}.

params(Job) when is_record(Job, job) ->
	{Job#job.freq, Job#job.c, Job#job.rho}.

geoms(Job) when is_record(Job, job) ->
	Job#job.geoms.

refl(Job) when is_record(Job, job) ->
	Job#job.refl.

test() ->
	ok.
