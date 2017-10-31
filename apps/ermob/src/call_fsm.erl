-module(call_fsm).
-behaviour(gen_statem).

%% public API
-export([start/0, start_link/0, dial/2, hangup/0]).
%% gen_statem callbacks
-export([init/1, terminate/3, code_change/4,callback_mode/0,
         % custom state names
         idle/3,  wait4_dialing/3, wait4_ring/3,
         wait4_ans/3, connected/3, wait4_release/3]).
-define (RING_TIME, 35000).
-define (DIAL_TIME, 30000).
-define (REL_TIME, 10000).
-define (ANS_TIME, 30000).
-define (ESTABLISH_TIME, 20000).
-record(state, {port,
		device,
    parser,
		from,
		number,
    t_start,
		tini,
		tring,
		tans,
		thangup,
		maxtime,
		tref,
		rel_party,
		in_call,
    buffer}).

%%% PUBLIC API
start() ->
    gen_statem:start(?MODULE, [], []).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

dial ( Number, MaxTime) ->
    gen_statem:call(cc_fsm,{dial, {Number,MaxTime} }, MaxTime + 50000).

hangup () ->
    gen_statem:call(cc_fsm,hangup).

%%% GEN_FSM API
init(Args) ->
    Dev    = proplists:get_value(device, Args),
    Parser = proplists:get_value(serial_parser, Args),
    SerialPort = serial:start([{open, Dev}, {speed, 115200}]),
    State = #state{ port   =SerialPort,
		                device = Dev,
                    parser = Parser,
                    number = "0",
    		            tini   = -1,
		                tring  = -1,
		                tans   = -1,
		                rel_party = "called", 
		                in_call   = false, 
                    buffer    = <<>>},
    register(cc_fsm,self()),
    {ok, idle,State,hibernate}.

callback_mode() -> state_functions.

terminate(Reason, StateName,_Data) ->
    error_logger:info_msg("Terminate Call Control FSM [~p][~p]\n",[StateName,Reason]),
    ok.

code_change(_OldVsn, StateName, Data,_Extra ) ->
    {ok, StateName, Data}.

%% States
idle( cast, ok , S)->
    call_fsm:info("Ok received, probably an external cmd ... \n"),
    {keep_state, S,hibernate};
idle( cast, {error,Cause} , S)->
    call_fsm:info("[idle] Error [~p]\n",[Cause]),
    {keep_state, S,hibernate};
idle( cast, ring , S)->
    call_fsm:info("Incomming call, rejecting ... \n"),
    #state{ tref   = TRef,
            parser = Parser,
            port   = SerialPort} = S,
    timer:cancel(TRef),
    Ndata = S#state{in_call = true},
    end_call(SerialPort,Parser),
    {next_state, wait4_release, Ndata,{timeout,?REL_TIME,wait4_release} };
idle( {call,From},{dial,{Number, CallTime}}, S)->
    Cmd = string:concat("ATD",Number),
    Bytes = string:concat(Cmd,";\r"),
    %{ok, TRef} = timer:send_after(?ESTABLISH_TIME, self(), est_timeout),
    Tini = erlang:system_time(millisecond),
    #state{port=SerialPort} = S,
    Ns   = S#state{ number = Number,
	                  maxtime = CallTime,
	                  t_start = vutils:get_date(),
	                  tini    = Tini,
	                  in_call = false,
                    tring = -1,
                    tans = -1,
                    rel_party = "called",
	                  from    = From},
    SerialPort ! {send,Bytes},         
    {next_state, wait4_dialing, Ns,{state_timeout, ?DIAL_TIME, wait4_dialing}};
idle(info, {closed, Reason}, Sdata) ->
    #state{device=Dev} = Sdata,
    call_fsm:info("[call_fsm] Closed received in Idle [~p]\n",[Reason]),
    SerialPort = serial:start([{open, Dev}, {speed, 115200}]),
    Data = Sdata#state{port=SerialPort},
    {keep_state,Data};
idle(EventType, EventContent, Data)->
    call_fsm:info("[call_fsm] Data received in Idle [~p][~p]\n",[EventType, EventContent]),
    Wtimeout = {timeout,infinity,idle_timeout},
    handle_event(EventType, EventContent, Data, Wtimeout).

wait4_dialing(cast, ok, S)->
    call_fsm:info("Ok Received"),
    {keep_state, S,{state_timeout, ?DIAL_TIME, wait4_dialing} };
wait4_dialing(cast, dialing, S)->
    call_fsm:info("Dialing Received"),
    {next_state, wait4_ring, S, {state_timeout, ?RING_TIME, wait4_dialing} };
wait4_dialing(cast, {error,Cause}, S)->
    call_fsm:info("[dialing] Error [~p]\n",[Cause]),
    Ret = error_return(S),
    Response = {reply,S#state.from,{ok,Ret}},
    {next_state, idle, S,[Response,hibernate]};
wait4_dialing(EventType, EventContent, Data)->
    Wtimeout = {state_timeout,?DIAL_TIME,wait4_dialing},
    handle_event(EventType, EventContent, Data, Wtimeout).

wait4_ring (cast, ringing, S)->
    Tnow = erlang:system_time(millisecond),
    #state{tini = Tini} = S,
    Ns   = S#state{tring = Tnow - Tini},
    call_fsm:info("Ringing Received"),
    {next_state, wait4_ans, Ns,{state_timeout, ?ANS_TIME, wait4_ans} };
wait4_ring (cast, connected, S)->
    Tnow = erlang:system_time(millisecond),
    call_fsm:info("Connected Received"),
    #state{tini    = Tini,
           maxtime = MTime} = S,
    Ns   = S#state{tans = Tnow - Tini},
    {next_state, connected, Ns,{timeout,MTime,call_timeout}};
wait4_ring(EventType, EventContent, Data)->
    Wtimeout = {state_timeout,?RING_TIME,wait4_ring},
    handle_event(EventType, EventContent, Data, Wtimeout).

wait4_ans(cast, connected, S)->
    Tnow = erlang:system_time(millisecond),
    call_fsm:info("Connected Received"),
    #state{tini    = Tini,
           maxtime = MTime} = S,
    Ns   = S#state{tans = Tnow - Tini},
    {next_state, connected, Ns,{timeout,MTime,call_timeout}};
wait4_ans(EventType, EventContent, Data)->
    Wtimeout = {state_timeout,?ANS_TIME,wait4_ans},
    handle_event(EventType, EventContent, Data, Wtimeout).

connected( timeout, call_timeout, S )->
    %call_fsm:info("[connected] Event [~p]\n",[Event]),
    call_fsm:info("Ending Call after max time reached\n"),
    #state{ port   = SerialPort,
            parser = Parser} = S,
    Ndata = S#state{rel_party = "calling"},
    end_call(SerialPort, Parser),
    {next_state, wait4_release, Ndata,{timeout, ?REL_TIME, wait4_release} };
connected(EventType, EventContent, Data)->
    Tnow = erlang:system_time(millisecond),
    #state{tini    = Tini,
           tans    = Tans,
           maxtime = MTime} = Data,
    Diff = MTime - (Tnow - (Tini + Tans) ),
    RemTime = if Diff >= 0 -> Diff;
                 Diff < 0  -> 0
              end,
    Wtimeout = {timeout,RemTime,call_timeout},
    handle_event(EventType, EventContent, Data, Wtimeout).
%% TODO pendiente connected y idle 2017/09/17
wait4_release(cast, ok, S)->
    {keep_state, S,{timeout, ?REL_TIME, wait4_release}};
wait4_release(EventType, EventContent, Data)->
    Wtimeout = {timeout,?REL_TIME,wait4_release},
    handle_event(EventType, EventContent, Data, Wtimeout).

handle_event(cast, {cend,Cause}, Data, _T) ->
    call_fsm:info("Cend received cause [~p]\n",[Cause]),
    Tnow = erlang:system_time(millisecond),
    #state{ tini = Tini,
            parser = Parser} = Data,
    Res = create_call_info (Data,Cause, Parser),
    Ns   = Data#state{thangup = Tnow - Tini},
    Response = {reply, Ns#state.from , {ok,Res}},
    {next_state, idle, Ns,[Response,hibernate]};
handle_event(cast, hangup, Data, T) ->
    call_fsm:info("Ending Call,hangup received state\n"),
    #state{ port   = SerialPort,
            parser = Parser} = Data,
    Ndata = Data#state{rel_party = "calling"},
    end_call(SerialPort, Parser),
    {next_state, wait4_release, Ndata, T };
handle_event( state_timeout, EventInfo, Sdata ,T) ->
    call_fsm:info("Time out event info state [~p]\n",[EventInfo]),
    #state{ port   = SerialPort,
            parser = Parser} = Sdata,
    Ndata = Sdata#state{rel_party = "calling"},
    end_call(SerialPort, Parser),
    {next_state, wait4_release, Ndata, T};
handle_event( timeout, wait4_release, S, _T ) ->
    call_fsm:info("Timeout in wait4_release going idle and reply\n"),
    Ret = error_return(S),
    Response = {reply,S#state.from,{ok,Ret}},
    {next_state, idle, S,[Response,hibernate]};
handle_event(info, {closed, Reason}, Sdata,_T) ->
    call_fsm:info(" Closed received reason [~p]\n",[Reason]),
    #state{device=Dev} = Sdata,
    SerialPort = serial:start([{open, Dev}, {speed, 115200}]),
    Ndata = Sdata#state{port=SerialPort},
    Ret = error_return(Ndata),
    Response = {reply,Ndata#state.from,{ok,Ret}},
    {next_state, idle, Ndata, [Response,hibernate]};
handle_event(info, {data, Bytes}, Sdata, WaitTime) ->
    call_fsm:info("Data received [~p]\n",[Bytes]),
    #state{ buffer = B,
            parser = Parser} =  Sdata,
    Data = << B/binary, Bytes/binary >>,
    Rest = proc_data(Data,Parser),
    Ndata = Sdata#state{buffer = Rest},
    {keep_state, Ndata, WaitTime};
handle_event(cast, close, Sdata,_T) ->
    #state{port=SerialPort} = Sdata,
    SerialPort ! {close},
    {next_state,idle, hibernate};
handle_event(Event, EventContent, Data,Timeout) ->
    call_fsm:info("Unexpected Event [~p][~p]\n",[Event,EventContent]),
    {keep_state,  Data, Timeout}.


%%% Internal functions

%% Process data from serial port

proc_data (Buff,Parser) ->
  Ret = binary:split(Buff,<<"\r\n">>),
  proc_data_r (Ret,Parser).

proc_data_r ([<<>>,Rem],Parser)->
  Ret = binary:split(Rem,<<"\r\n">>),
  proc_data_r (Ret,Parser);
proc_data_r ([Msg,Rem],Parser)->
  Response = serial_utils:split_response(Msg),
  Event = Parser:call_event(Response),
  gen_statem:cast(self(),Event),
  Ret = binary:split(Rem,<<"\r\n">>),
  proc_data_r (Ret,Parser);
proc_data_r ([<<>>],_)->
  <<>>;
proc_data_r ([Rem],_)->
  Rem.

end_call(SerialPort, Parser)->
    Bytes = Parser:end_cmd(),
    SerialPort ! {send,Bytes}.

create_call_info (State,Cend,Parser)->
  EndInfo = Parser:parse_cend(Cend),
  #state{
          t_start   = Tstart,
	        tring     = Ring,
	        rel_party = Relp,
	        tans      = Ans} = State,
  Ret = [ {t_start, Tstart},
	        {rel_party,Relp},
	        {tring,Ring},
	        {tans,Ans}
        ],
  lists:append(Ret,EndInfo).

error_return (S) ->
   #state{
         t_start = Tstart,
         tring   = Ring,
         rel_party   = Relp,
         tans    = Ans} = S,
    Res = [ {t_start, Tstart},
          {rel_party,Relp},
          {tring,Ring},
          {tans,Ans},
          {duration, 0},
          {end_status,-1},
          {cause,-1}
        ],
     Res.

