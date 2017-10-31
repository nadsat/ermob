%%%-------------------------------------------------------------------
%% @doc upload public API
%% @end
%%%-------------------------------------------------------------------

-module(uplink).

-behaviour(gen_server).

%% Application callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
-export([run/3]).

-record(state,
         {from,
          con_number,
          wait_time,
          t_ini,
          dev_id,
          t_start,
          test_id,
          volume,
          t_up,
          speed,
          host_ip,
          url_dest,
          type
          }).
%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).


run (MaxTime, Uuid,Args)->
    gen_server:call(?MODULE,{upload,MaxTime, Uuid,Args},MaxTime + 2000).

init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("Uplink init\n"),
    Devid = vutils:get_devid(),
    State = #state{dev_id = Devid,
                   con_number = -1
                   },
    register(?MODULE,self()),
    {ok, State,hibernate}.

handle_info({done,B}, S) when S#state.con_number == 0 ->
    Bytes  = B + S#state.volume,
    Dtime  = erlang:system_time(millisecond)-S#state.t_ini,
    Speed  = Bytes*8 div Dtime, % Kbps
    NState = S#state{
              volume = Bytes,
              t_up = Dtime,
              speed  = Speed},
    Res    = create_res (NState),
    gen_server:reply(NState#state.from, {uplink,Res}),
    {noreply, NState#state{con_number = -1},hibernate};
handle_info({done,B}, #state{volume = V, con_number=CN} = S) when S#state.con_number > 0 ->
    NState = S#state{volume = V+B, con_number=CN-1},
    WaitTime = S#state.wait_time - (erlang:system_time(millisecond)-S#state.t_ini),
    {noreply, NState,WaitTime};
handle_info({done,_}, State) when State#state.con_number < 0 ->
    error_logger:info_msg("[downlink] Done msg out of time\n"),
    {noreply, State,hibernate};
handle_info(timeout, S) ->
    Bytes  = S#state.volume,
    Dtime  = erlang:system_time(millisecond)-S#state.t_ini,
    Speed  = Bytes*8 div Dtime, % Kbps
    NState = S#state{
              volume = Bytes,
              t_up = Dtime,
              speed  = Speed},
    Res    = create_res (NState),
    gen_server:reply(NState#state.from, {uplink,Res}),
    {noreply, NState#state{con_number = -1},hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State,hibernate}.

handle_call({upload,MaxTime, Uuid,Args}, From,State)  ->
    Url       = proplists:get_value(url, Args),
    ConNumber = proplists:get_value(con_number, Args),
    Filesize  = proplists:get_value(filesize, Args), 
    Iface     = proplists:get_value(iface, Args),
    Type      = proplists:get_value(type, Args),

    IP    = vutils:get_ipv4(Iface),
    Date  = vutils:get_date(),
    Tini = erlang:system_time(millisecond),
    NState = State#state{
                volume  = 0,
                t_ini   = Tini,
                t_start = Date,
                test_id = Uuid,
                host_ip = IP,
                url_dest = Url,
                from = From,
                wait_time = MaxTime,
                con_number = ConNumber-1,
                type = Type},
    dispatch_worker(Url,ConNumber,Filesize),
    {noreply, NState,MaxTime}.


terminate(_Reason, _State) ->
    error_logger:info_msg("Leaving  down_measure\n"),
    ok.

code_change(_OldVsn, State, Data ) ->
    {ok, State, Data}.


%%====================================================================
%% Internal functions
%%====================================================================

create_res (State)->
    #state{ dev_id   = Devid,
            t_start  = Date,
            test_id  = Uuid,
            volume   = Bytes,
            t_up   = Dtime,
            speed    = Speed,
            host_ip  = IP,
            url_dest = Url,
            type     = Type} = State,
          [ {dev_id,Devid},{t_start,Date},
            {test_id,Uuid},{volume,Bytes},
            {t_up,Dtime},{speed,Speed},
            {host_ip,IP},{url_dest,Url},
            {type,Type}
          ].

dispatch_worker(Url,ConNumber,Filesize)->
    exec_task (ConNumber, Url,Filesize).

exec_task (1, Url,Filesize)->
    Pid = self(),
    spawn(fun () -> poolboy:transaction(uplink_pool, fun(Worker) ->
                          gen_server:call(Worker, {up, Url,Filesize,Pid},infinity)
                      end)
              end),
    ok;
exec_task (Count, Url,Filesize)->
    Pid = self(),
    spawn(fun () ->
                  poolboy:transaction(uplink_pool, fun(Worker) ->
                          gen_server:call(Worker, {up, Url,Filesize,Pid},infinity)
                      end)
         end),
    exec_task (Count-1,Url,Filesize).
