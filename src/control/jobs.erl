%Aclevsim
%David Honour 2013
%GPL v3
%
%Job control module:
%
-module(jobs).
-export([start/0, add/3, remove/2, list/1, task_get/1, task_done/1, task_fail/1, jobcontrol/2, test/0]).

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

-record(job, {method, tasks=[]}).
-record(jtasks, {active=orddict:new(), pending=[], complete=[]}).

entask(Job) ->
	{ok, ['a','b','c']}.

%Job control functions

jc_add(From, Jid, Job, JD, {ok, Tasks}) ->
	From ! {self(), ok},
	orddict:store(Jid, Job#job{tasks=Tasks}, JD);
jc_add(From, _, _, JD, {error, Msg}) ->
	From ! {self(), {error, Msg}},
	JD.

%Job control task functions

jct_add(JC, {ok, Tasks}) ->
	JC#jtasks{pending=JC#jtasks.pending ++ Tasks};
jct_add(JC, {error, _}) ->
	JC.

jct_rm(Tasks, Msg, Ord) ->
	Pred = fun(From, Task) ->
		case lists:member(Task, Tasks) of
			true ->
				From ! {self(), Msg},
				false;
			false ->
				true
		end
	end,
	orddict:filter(Pred, Ord).

jct_remove(JC, Job) ->
	Tasks = Job#job.tasks,
	Active = jct_rm(Tasks, stop, JC#jtasks.active),
	%I think the -- operation is in C for speed, so maybe it would be better to do that first and remove results (if speed is a concern)
	JC#jtasks{pending=JC#jtasks.pending -- Tasks, complete=JC#jtasks.complete -- Tasks, active=Active}.

jct_assign(From, JC) when length(JC#jtasks.pending) > 0 ->
	[Task|Others] = JC#jtasks.pending,
	From ! {self(), {start, Task}},
	JC#jtasks{active=orddict:store(From, Task, JC#jtasks.active), pending=Others};
jct_assign(From, JC) ->
	From ! {self(), notasks},
	JC.

jct_complete(From, JC) ->
	Task = [orddict:fetch(From, JC#jtasks.active)],
	Active = jct_rm(Task, ok, JC#jtasks.active),
	JC#jtasks{active=Active, complete=JC#jtasks.complete ++ Task}.

jct_repool(From, JC) ->
	Task = [orddict:fetch(From, JC#jtasks.active)],
	Active = jct_rm(Task, ok, JC#jtasks.active),
	JC#jtasks{active=Active, pending=JC#jtasks.pending ++ Task}.

%Job control

jobcontrol(JD, JC) ->
	receive
		{From, {add, Jid, Job}} ->
			case orddict:is_key(Jid, JD) of
				true ->
					From ! {self(), {error, "Invalid Job ID"}},
					jobcontrol(JD, JC);
				false ->
					Tasks = entask(Job),
					jobcontrol(jc_add(From, Jid, Job, JD, Tasks), jct_add(JC, Tasks))
			end;
		{From, {remove, Jid}} -> %Add a check for valid from
			From ! {self(), ok},
			jobcontrol(orddict:erase(Jid, JD), jct_remove(JC, orddict:fetch(Jid, JD)));
		{From, list} ->
			From ! {self(), {list, orddict:to_list(JD)}},
			jobcontrol(JD, JC);
		{From, {task, ready}} ->
			jobcontrol(JD, jct_assign(From, JC));
		{From, {task, ok}} ->
			jobcontrol(JD, jct_complete(From, JC));
		{From, {task, error}} ->
			jobcontrol(JD, jct_repool(From, JC));
		{From, inspect} ->
			From ! {JD, JC},
			jobcontrol(JD, JC);
		terminate ->
			ok
	end.

%Interface functions

start() ->
	spawn(?MODULE, jobcontrol, [orddict:new(), #jtasks{active=orddict:new()}]).

add(Pid, Jid, Job) ->
	Pid ! {self(), {add, Jid, Job}},
	receive
		{Pid, ok} -> ok;
		{Pid, {error, Msg}} -> {error, Msg}
	end.

remove(Pid, Jid) ->
	Pid ! {self(), {remove, Jid}},
	receive
		{Pid, ok} -> ok
	end.

list(Pid) ->
	Pid ! {self(), list},
	receive
		{Pid, {list, Msg}} -> Msg
	end.

task_get(Pid) ->
	Pid ! {self(), {task, ready}},
	receive
		{Pid, {start, Task}} -> {ok, Task};
		{Pid, notasks} -> {error, notasks}
	end.

task_done(Pid) ->
	Pid ! {self(), {task, ok}},
	receive
		{Pid, ok} -> ok
	end.

task_fail(Pid) ->
	Pid ! {self(), {task, error}},
	receive
		{Pid, ok} -> ok
	end.

%Testing

test() ->
	%Start
	J = start(),
	%Add
	ok = add(J, "Test1", #job{method="Somemethod"}),
	ok = add(J, "Test2", #job{method="Somemethod"}),
	{error, "Invalid Job ID"} = add(J, "Test2", #job{method="Somemethod"}),
	%List
	[_, _] = list(J), %Not exactly exaustive
	%Task functions
	{ok, 'a'} = task_get(J),
	ok = task_done(J),
	{ok, 'b'} = task_get(J),
	ok = task_fail(J),
	{ok, 'c'} = task_get(J),
	%Remove
	ok = remove(J, "Test1"),
	receive
		{J, stop} -> ok
	end,
	%More task functions
	{ok, 'b'} = task_get(J),
	ok = task_done(J),
	{error, notasks} = task_get(J),
	ok.
	%J ! {self(), inspect}.
