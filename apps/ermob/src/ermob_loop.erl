-module(ermob_loop).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).

-record(state,{tlist,
               scheduler}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    %application:start(crypto),
    {ok, Scheduler} = application:get_env(scheduler),
    {ok, TList}      = application:get_env(tests),
    State = #state{tlist=TList,
                   scheduler=Scheduler},
    Wtime = get_waiting_time(Scheduler,0),
    erlang:send_after(Wtime, self(), run_task_list),
    {ok, State, hibernate}.

handle_info(run_task_list, State) ->
    #state{tlist=TList,
                   scheduler=Scheduler} = State,
    Tini = erlang:system_time(seconds),
    Uuid = uuid(),
    run_task (TList,Uuid),
    Tend = erlang:system_time(seconds),
    Diff = Tend - Tini,
    Wtime = get_waiting_time(Scheduler,Diff),
    %db:insert_data(Wtime - 20), %this should be enabled soon 
    Tdb = erlang:system_time(seconds),
    Ftime = Wtime - (Tdb -Tend),
    erlang:send_after(Ftime, self(), run_task_list),
    {noreply, State, hibernate};
handle_info(Msg, State) ->
    error_logger:info_msg("Unknown Msg on vime_loop [~p]\n",[Msg]),
    {noreply, State, hibernate}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(incomplete, State)  ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("Leaving  vime_loop\n"),
    ok.

code_change(_OldVsn, State, Data ) ->
    {ok, State, Data}.

%Internal functions
run_task ([],_U) ->
    error_logger:info_msg("Running Tasks Done\n");

run_task ([H|T], Uuid) ->
    {M,A,{max_test_time,Mt}} = H,
    Args = [Mt| [ Uuid|[A] ] ],
    error_logger:info_msg("Running Task [~p]\n",[M]),
    Ret = apply(M,run,Args),
    error_logger:info_msg("Task Result [~p]\n",[Ret]),
    run_task(T,Uuid).
    
get_waiting_time ({period,Time},RTime) ->

  Period = Time - RTime,
  case Period > 0 of
    true  -> Period;
    false -> 0
  end.

% Generates a random binary UUID.
% Returns a string representation of a binary UUID.
uuid() ->
    U = v4(),
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U))).
v4() ->
  v4(rand:uniform(round(math:pow(2, 48))) - 1, rand:uniform(round(math:pow(2, 12))) - 1,
     rand:uniform(round(math:pow(2, 32))) - 1, rand:uniform(round(math:pow(2, 30))) - 1).

v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.


% Returns the 32, 16, 16, 8, 8, 48 parts of a binary UUID.
get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
[TL, TM, THV, CSR, CSL, N].
