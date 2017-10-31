%%%-------------------------------------------------------------------
%% @doc delay public API
%% @end
%%%-------------------------------------------------------------------

-module(rtt).

-behaviour(gen_server).

%% Application callbacks
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
-export([run/3]).
-record(state,
         {port,
          from,
          data,
          t_ini,
          wait_time,
          dev_id,
          t_start,
          test_id,
          pack_trans,
          pack_recv,
          rtt_min,
          rtt_avg,
          rtt_max,
          rtt_stsdev,
          type
          }).
%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).


run (MaxTime, Uuid,Args)->
    gen_server:call(?MODULE,{latency,MaxTime, Uuid,Args},MaxTime + 2000).

init([]) ->
    process_flag(trap_exit, true),
    error_logger:info_msg("Downlink init\n"),
    Devid = vutils:get_devid(),
    State = #state{dev_id = Devid,
                   port = -1,
                   data = "" },
    register(?MODULE,self()),
    {ok, State,hibernate}.

handle_info({'EXIT',_, _}, S)  ->
    {noreply, S,hibernate};
handle_info({Port, {data, Data}}, #state{data=D}=S) when S#state.port == Port ->
    NState = S#state{data = D++Data},
    WaitTime = S#state.wait_time - (erlang:system_time(millisecond)-S#state.t_ini),
    {noreply, NState,WaitTime};
handle_info({Port, {exit_status, 0}}, #state{data=D}=S) when S#state.port == Port ->
    [Line1,Line2,_LineTrail] = re:split(D, "\n"),
    Line1Split = re:split(Line1, " "),
    PackTrans = binary_to_integer( lists:nth(1,Line1Split) ),
    PackRecv = binary_to_integer( lists:nth(4,Line1Split) ),
    {RTTMin,RTTAvg,RTTMax,RTTDev} = rtt_parser(Line2), 
    NState = S#state{ port =-1,
                      pack_trans = PackTrans,
                      pack_recv = PackRecv,
                      rtt_min = RTTMin,
                      rtt_avg = RTTAvg,
                      rtt_max = RTTMax,
                      rtt_stsdev = RTTDev},
    Res    = create_res (NState),
    gen_server:reply(NState#state.from, {rtt,Res}),
    {noreply, NState,hibernate};
handle_info({Port, {exit_status, _}}, S) when S#state.port == Port ->
    error_logger:info_msg("[rtt] Ping Call returns an error\n"),
    Res    = create_res (S),
    gen_server:reply(S#state.from, {rtt,Res}),
    {noreply, S,hibernate};
handle_info({Port, _}, S) when S#state.port /= Port ->
    error_logger:info_msg("[rtt] Return msg out of time\n"),
    Res    = create_res (S),
    gen_server:reply(S#state.from, {rtt,Res}),
    {noreply, S,hibernate};
handle_info(timeout, S) ->
    Res    = create_res (S),
    gen_server:reply(S#state.from, {rtt,Res}),
    {noreply, S,hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State,hibernate}.

handle_call({latency,MaxTime, Uuid,Args}, From,State)  ->
    Url       = proplists:get_value(url, Args),
    PackCount = proplists:get_value(packet_number, Args),
    Type      = proplists:get_value(type, Args),
    Op        = [{timeout,10000},{connect_timeout,5000}],
    Sample = "http://sp1.wom.cl/speedtest/random1000x1000.jpg",
    case httpc:request(get, {Sample, []}, Op, []) of
      {ok, {{_Version, _RespCode, _ReasonPhrase}, _Headers, _Body}} -> ok;
      {ok, _a, _b, _c} -> ok;
      {error,timeout} -> ok;
      {error, _e} -> error_logger:info_msg("Dummy download returned ERROR. Continuing with delay test")
    end,
    Date  = vutils:get_date(),
    Cmd = lists:concat(["ping  -c ",PackCount," ",Url," | tail -2 "]),
    Tini = erlang:system_time(millisecond),
    Port = erlang:open_port({spawn, Cmd},[exit_status]),
    NState =  State#state{
                data    = "",
                from = From,
                port = Port,
                t_ini   = Tini,
                t_start = Date,
                test_id = Uuid,
                pack_trans = 0,
                pack_recv = 0,
                rtt_min = 0.0,
                rtt_avg = 0.0,
                rtt_max = 0.0,
                rtt_stsdev = 0.0,
                type = Type,
                wait_time = MaxTime},
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
  #state{ dev_id = Devid,
          t_start = Date,
          test_id = Uuid,
          pack_trans = PackTrans,
          pack_recv = PackRecv,
          rtt_min = RTTMin,
          rtt_avg = RTTAvg,
          rtt_max = RTTMax,
          rtt_stsdev = RTTDev,
          type = Type} = State,
  [ {dev_id,Devid},
    {t_start,Date},
    {test_id,Uuid},
    {pack_trans,PackTrans},
    {pack_recv,PackRecv},
    {rtt_min,RTTMin},
    {rtt_avg,RTTAvg},
    {rtt_max,RTTMax},
    {rtt_stsdev,RTTDev},
    {type,Type}
   ].

rtt_parser (<<>>) ->
   {0.0, 0.0, 0.0, 0.0};
rtt_parser (Line) ->
   Line2Split = re:split(Line, "/"),
   RTTMin = binary_to_float( lists:nth(3, re:split(lists:nth(4,Line2Split)," ") ) ),
   RTTAvg = binary_to_float( lists:nth(5,Line2Split) ),
   RTTMax = binary_to_float( lists:nth(6,Line2Split) ),
   RTTDev = binary_to_float( lists:nth(1, re:split(lists:nth(7,Line2Split)," ") ) ),
   {RTTMin,RTTAvg,RTTMax,RTTDev}.
