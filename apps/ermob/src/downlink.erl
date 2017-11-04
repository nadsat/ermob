%%%-------------------------------------------------------------------
%% @doc download public API
%% @end
%%%-------------------------------------------------------------------

-module(downlink).

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
          t_down,
          speed,
          host_ip,
          url_dest,
          type,
          ttfb
          }).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).


run (MaxTime, Uuid,Args)->
    gen_server:call(?MODULE,{download,MaxTime, Uuid,Args},MaxTime + 2000).

init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("Downlink init\n"),
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
              t_down = Dtime,
              speed  = Speed},
    Res    = create_res (NState),
    gen_server:reply(NState#state.from, {downlink,Res}),
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
              t_down = Dtime,
              speed  = Speed},
    Res    = create_res (NState),
    gen_server:reply(NState#state.from, {downlink,Res}),
    {noreply, NState#state{con_number = -1},hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State,hibernate}.

handle_call({download,MaxTime, Uuid,Args}, From,State)  ->
    Url       = proplists:get_value(url, Args),
    ConNumber = proplists:get_value(con_number, Args),
    Iface     = proplists:get_value(iface, Args),
    Type      = proplists:get_value(type, Args),
    Op        = [{timeout,10000},{connect_timeout,5000}],
    {ok,_Ref} = httpc:request(head,{Url,[]},Op,[{sync,false},{stream,self}]),
    T0    = erlang:system_time(millisecond),
    Len   = http_ans (),
    T1    = erlang:system_time(millisecond),
    Ttfb  = T1 - T0,
    IP    = vutils:get_ipv4(Iface),
    Date  = vutils:get_date(),
    Tini = erlang:system_time(millisecond),
    TmpState = State#state{
                volume  = 0,
                t_ini   = Tini,
                t_start = Date,
                test_id = Uuid,
                host_ip = IP,
                url_dest = Url,
                type = Type,
                ttfb = Ttfb},
    case Len > 0 of
      true -> dispatch_worker(Url,ConNumber,Len),
              NState = TmpState#state{
                        from = From,
                        wait_time = MaxTime,
                        con_number = ConNumber-1},
              {noreply, NState,MaxTime};
      false-> NState = TmpState#state{
                        con_number = -1,
                        t_down = -1,
                        speed  = 0},
              Res = create_res (NState),
              error_logger:info_msg("Len less than 0  [~p]\n",[Len]),
              {reply,{downlink,Res}, NState,hibernate}
    end.
    
terminate(_Reason, _State) ->
    error_logger:info_msg("Leaving  down_measure\n"),
    ok.

code_change(_OldVsn, State, _Data ) ->
    {ok, State}.
%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================

create_res (State)->
    #state{ dev_id   = Devid,
            t_start  = Date,
            test_id  = Uuid,
            volume   = Bytes,
            t_down   = Dtime,
            speed    = Speed,
            host_ip  = IP,
            url_dest = Url,
            type     = Type,
            ttfb     = Ttfb} = State,
          [ {dev_id,Devid},{t_start,Date},
            {test_id,Uuid},{volume,Bytes},
            {t_down,Dtime},{speed,Speed},
            {host_ip,IP},{url_dest,Url},
            {type,Type},{ttfb,Ttfb}
          ].

dispatch_worker(Url,ConNumber,Len)->
    Step = Len div ConNumber,
    exec_task (ConNumber, Url,0,Step,Len).

exec_task (1,Url,Start,_Step,Last)->
    R = io_lib:format("bytes=~w-~w",[Start,Last]),
    Pid = self(),
    spawn(fun () -> poolboy:transaction(downlink_pool, fun(Worker) ->
                          gen_server:call(Worker, {down, Url,R,Pid},infinity)
                      end)
              end),
    ok;
exec_task (Count,Url,Start,Step,Last)->
    Next = Start + Step,
    R = io_lib:format("bytes=~w-~w",[Start,Next]),
    Pid = self(),
    spawn(fun () ->
                  poolboy:transaction(downlink_pool, fun(Worker) ->
                          gen_server:call(Worker, {down, Url,R,Pid},infinity)
                      end)
         end),
    exec_task (Count-1,Url,Next+1,Step,Last).

%proc_task (Url,Range)->
%    poolboy:transaction(download fun(Worker) ->
%                          gen_server:call(Worker, {down, Url,Range,self()})
%                      end).

http_ans () ->
  http_data (-1).

http_data (N) ->
    receive
      {http, {RId, {error, Reason}}}      ->  error_logger:info_msg("Error [~p][~p][~p]\n",[RId,Reason,N]),
                                              N;
      {http, {RId, Result}}               ->  error_logger:info_msg("Result --[~p]-- [~p]\n",[RId,Result]),
                                              N;
      {http, {_RId, stream_start, Headers}}->  Len = get_length(Headers),
                                              http_data(Len);
      {http, {_RId, stream_end, _Headers}}  ->  N
    end.
get_length (H)->
  case lists:keyfind("content-length",1,H) of
    false  -> -1;
    {_K,V} -> lists:keyfind("content-length",1,H),
              {Len, _Rest} = string:to_integer(V),
              Len
   end.
