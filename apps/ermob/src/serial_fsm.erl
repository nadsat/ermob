-module(serial_fsm).
-behaviour(gen_statem).

%% public API
-export([start/0, start_link/0, set_cmd/1, ask_cmd/1]).
%% gen_statem callbacks
-export([init/1, handle_event/3, 
         terminate/3, code_change/4,callback_mode/0,
         % custom state names
         idle/3,  wait4_ok/3, wait4_ans/3]).

-record(state, {port,
                parser,
		            device,
		            from,
                command,
                response,
                buffer}).

%%% PUBLIC API
start() ->
    gen_statem:start(?MODULE, [], []).

start_link() ->
    gen_statem:start_link(?MODULE, [], []).

set_cmd (Cmd) ->
    gen_statem:call(fsm,{set_at, Cmd},20000).
ask_cmd (Cmd) ->
    gen_statem:call(fsm,{ask_at, Cmd},20000).

%%% GEN_FSM API
init(Args) ->
    Dev    = proplists:get_value(device, Args),
    Parser = proplists:get_value(serial_parser, Args),
    SerialPort = serial:start([{open, Dev}, {speed, 115200}]),
    State = #state{ port = SerialPort,
                    parser = Parser,
		                device = Dev,
                    command = none,
                    buffer= <<>>},
    register(fsm,self()),
    error_logger:info_msg("Registered Serial [~p]\n",[self()]),
    {ok, idle, State, hibernate}.

callback_mode() -> state_functions.

terminate(Reason, StateName,_Data) ->
    error_logger:info_msg("Terminate Serial FSM [~p][~p]\n",[StateName,Reason]),
    ok.

code_change(_OldVsn, StateName, Data,_Extra ) ->
    {ok, StateName, Data}.

%% States
idle({call,From},{set_at,Bytes}, #state{parser = Parser} = S)->
    {Cmd,_P} = Parser:split_cmd(Bytes),
    Ns = S#state{command = Cmd, 
               from = From, 
               response = <<>>},
    self() ! {send,Bytes},         
    {next_state, wait4_ok, Ns,{state_timeout,10000,wait4_ok}};
idle({call,From},{ask_at,Bytes}, #state{parser = Parser} = S)->
    {Cmd,_P} = Parser:split_cmd(Bytes),
    Ns = S#state{command = Cmd, 
               from = From, 
               response = <<>>},
    self() ! {send,Bytes},         
    {next_state, wait4_ans, Ns,{state_timeout,10000,wait4_ans}};
idle(info, {closed, Reason}, Sdata) ->
    #state{device=Dev} = Sdata,
    serial_cmd:info("[serial_fsm] Closed received in Idle [~p]\n",[Reason]),
    SerialPort = serial:start([{open, Dev}, {speed, 115200}]),
    Data = Sdata#state{port=SerialPort},
    {keep_state,Data};
idle(Event, Msg, Data)->
    serial_cmd:info("Unexpected Event Serial FSM Idle [~p][~p]\n",[Event,Msg]),
    {keep_state,Data}.

wait4_ok(cast,ok, S)->
    Response = {reply, S#state.from ,{ok,S#state.response}},
    {next_state, idle, S,[Response,hibernate]};
wait4_ok(cast,{error,P}, S)->
    Response = {reply,S#state.from,{error,P}},
    {next_state, idle, S,[Response,hibernate]};
wait4_ok(EventType, EventContent, Data)->
    handle_event(EventType, EventContent, Data).

wait4_ans(cast, ok, S)->
    {next_state, idle, S, hibernate};
wait4_ans(cast, {ans,Payload}, S)->
    Ns = S#state{response = Payload},
    {next_state, wait4_ok, Ns, {state_timeout,5000,wait4_ok}};
wait4_ans(cast, {error,Payload}, S)->
    Response = {reply,S#state.from , {error,Payload}},
    {next_state, idle, S,[Response,hibernate]};
wait4_ans(EventType, EventContent, Data)->
    handle_event(EventType, EventContent, Data).

handle_event(info, {closed, Reason}, Sdata) ->
    #state{device=Dev} = Sdata,
    serial_cmd:info("[serial_fsm] Closed received send reply [~p]\n",[Reason]),
    Response = {reply, Sdata#state.from , {error,Reason}},
    SerialPort = serial:start([{open, Dev}, {speed, 115200}]),
    Ndata = Sdata#state{port=SerialPort},
    {next_state, idle, Ndata, [Response,hibernate]};
handle_event(info,{data, Bytes}, Sdata) ->
    serial_cmd:info("Data received [~p]\n",[Bytes]),
    #state{ buffer = B,
            parser = Parser} =  Sdata,
    Data = << B/binary, Bytes/binary >>,
    Rest = proc_buffer(Data, Sdata#state.command, Parser),
    Ndata = Sdata#state{buffer = Rest}, 
    {keep_state, Ndata,{state_timeout,5000,unknown_event}};
handle_event(info, {send, Bytes}, Sdata) ->
    #state{port=SerialPort} = Sdata,
    SerialPort ! {send, Bytes},
    {keep_state, Sdata,hibernate};
handle_event(info, {close}, Sdata) ->
    #state{port=SerialPort} = Sdata,
    SerialPort ! {close},
    {next_state,idle, Sdata,hibernate};
handle_event( state_timeout,Ev , Sdata ) ->
    serial_cmd:info("Time out on state [~p]\n",[Ev]),
    Response = {reply, Sdata#state.from , {error,"Serial Timeout"}},
    {next_state, idle, Sdata, [Response,hibernate]};
handle_event(Event,  Msg, Data) ->
    serial_cmd:info("Unexpected Event Serial FSM [~p][~p]\n",[Event,Msg]),
    {keep_state,Data,{state_timeout,5000,unknown_event}}.


%%% Internal functions

%% Process data from serial port
%% Current represents last message sent and no answer received

proc_buffer (Buff, Current, Parser) ->
  Ret = binary:split(Buff,<<"\r\n">>),
  proc_buffer_r (Ret, Current, Parser).

proc_buffer_r ([<<>>,Rem], Current, Parser)->
  Ret = binary:split(Rem,<<"\r\n">>),
  proc_buffer_r (Ret, Current, Parser);
proc_buffer_r ([Msg,Rem], Current, Parser)->
  Response = Parser:split_response(Msg),
  Event = Parser:serial_event(Response,Current),
  gen_statem:cast(self(),Event),
  Ret = binary:split(Rem,<<"\r\n">>),
  proc_buffer_r (Ret,Current, Parser);
proc_buffer_r ([<<>>], _C, _)->
  <<>>;
proc_buffer_r ([Rem], _C, _)->
  Rem.



