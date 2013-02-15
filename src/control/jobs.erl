%Aclevsim
%David Honour 2013
%GPL v3
%
%Job control module:
%
-module(jobs).

%-compile(export_all).
-export([start/0, add/3, remove/2, list/1, get_task/1, task_done/1, task_fail/1]).

% Add job.
%  Analyse job.
%   Detect method.
%  Divide job into tasks (using method specific module).
%  Estimate task times (method specific).
%  Add to job list.
%  Add to pending task list.
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
%  Remove pending and complete tasks.
% Job priority.
%  Set low/high.
% Get task.
% Report completion.
% Report failure.

-record(job, {name, method, tasks=[]}).
-record(jtasks, {active=orddict:new(), pending=[], complete=[]}).

entask(Job) ->
	{ok, ['a','b','c']}.

%Job control functions

jc_add(From, Jid, Job, JD, {ok, Tasks}) ->
	From ! {self(), {ok, Jid}},
	orddict:store(Jid, Job#job{tasks=Tasks}, JD);
jc_add(From, _, _, JD, {err, Msg}) ->
	From ! {self(), {err, Msg}},
	JD.

%Job control task functions

jct_add(JC, JD, Jid) ->
	Tasks = orddict:fetch(Jid, JD)#job.tasks,
	JC#jtasks{pending=[JC#jtasks.pending | Tasks]}.

jct_foldrm(From, Task, {[Task|Others], Used, Acc, Msg}) ->
	%Task matches
	From ! Msg,
	{[Used|Others], [], Acc};
jct_foldrm(From, Task, {[], Used, Acc, Msg}) ->
	%Tasks list empty
	{Used, [], orddict:store(From, Task, Acc), Msg};
jct_foldrm(From, Task, {[Head|Others], Used, Acc, Msg}) ->
	%Task doesn't match but there are tasks in the list
	jct_foldrm(From, Task, {Others, [Used|Head], Acc, Msg}).

jct_rm(Tasks, Msg, Ord) ->
	{_, _, Active, _} = orddict:fold(jct_foldrm, {Tasks, [], orddict:new(), Msg}, Ord),
	Active.

jct_remove(JC, JD, Jid) ->
	Tasks = orddict:fetch(Jid, JD)#job.tasks,
	Active = jct_rm(Tasks, stop, JC#jtasks.active),
	%I think the -- operation is in C for speed, so maybe it would be better to do that first and remove results (if speed is a concern)
	JC#jtasks{pending=JC#jtasks.pending -- Tasks, complete=JC#jtasks.complete -- Tasks, active=Active}

jct_assign(From, JC#jtasks{pending=[]}) ->
	From ! ok,
	JC;
jct_assign(From, JC) ->
	[Task|Others] = JC#jtasks.pending,
	From ! {start, Task},
	JC#jtasks{active=orddict:store(From, Task, JC#jtasks.active), pending=Others}.

jct_complete(From, JC) ->
	Task = orddict:fetch(From, JC#jtasks.active),
	Active = jct_rm([Task], ok, JC#jtasks.active),
	JC#jtasks{active=Active, complete=[JC#jtasks.complete|Task]}.

jct_repool(From, JC) ->
	Task = orddict:fetch(From, JC#jtasks.active),
	Active = jct_rm([Task], ok, JC#jtasks.active),
	JC#jtasks{active=Active, pending=[JC#jtasks.pending|Task]}.

%Job control

jobcontrol(JD, JC) ->
	receive
		{From, {add, Jid, Job}} ->
			From ! "In",
			Key_exists = orddict:is_key(Jid, JD),
			From ! Key_exists,
			NJD = if Key_exists == true ->
					From ! {self(), {error, "Invalid Job ID"}};
				Key_exists == false ->
					jc_add(From, Jid, Job, JD, JC, entask(Job)),
					jct_add(JC, JD, Jid)
				end,
			jobcontrol(NJD);
		{From, {remove, Jid}} -> 
			From ! {self(), ok},
			jct_remove(JC, JD, Jid),
			jobcontrol(orddict:erase(Jid, JD));
		{From, list} ->
			From ! {self(), orddict:to_list(JD)},
			jobcontrol(JD);
		{From, {task, ready}} ->
			jobcontrol(JD, jct_assign(From, JC);
		{From, {task, ok}} ->
			jobcontrol(JD, jct_complete(From, JC);
		{From, {task, error}} ->
			From ! {self(), ok},
			jobcontrol(JD, jct_repool(From, JC));
		terminate ->
			ok
	end.

%Interface functions

start() ->
	spawn(?MODULE, jobcontrol, [orddict:new(), #jtasks{active=orddict:new()}]).

add(Pid, Jid, Job) ->
	Pid ! {self(), {add, Jid, Job}},
	receive
		{Pid, Msg} -> Msg
	end.

remove(Pid, Jid) ->
	Pid ! {self(), {remove, Jid}},
	receive
		{Pid, Msg} -> Msg
	end.

list(Pid) ->
	Pid ! {self(), list},
	receive
		{Pid, Msg} -> Msg
	end.

task_get(Pid) ->
	Pid ! {self(), {task, ready}},
	receive
		{start, Task} -> {ok, Task};
		ok -> {error, notasks}
	end.

task_done(Pid) ->
	Pid ! {self(), {task, ok}},
	receive
		ok -> ok
	end.

task_fail(Pid) ->
	Pid ! {self(), {task, error}},
	receive
		ok -> ok
	end.
