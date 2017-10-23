-module(down_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
-record(state,{c_timeout,
               d_timeout}).
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    Con_timeout  = proplists:get_value(con_timeout, Args),
    Down_timeout = proplists:get_value(down_timeout, Args),
    State = #state{c_timeout=Con_timeout,
                   d_timeout=Down_timeout},
    {ok, State,hibernate}.

handle_info(_Msg, State) ->
    {noreply, State,hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State,hibernate}.

handle_call({down,Url,Range,Pid}, _From,State)  ->
    #state{c_timeout=Con_timeout,
                   d_timeout=Down_timeout} = State,
    Op = [{timeout,Down_timeout},{connect_timeout,Con_timeout}],
    {ok,Ref} = httpc:request(get,{Url,[{"Range",Range}]},Op,[{sync,false},{stream,self}]),
    Bytes = receive_data(Ref,0),
    Pid ! {done,Bytes},
    {reply,Bytes, State,hibernate}.

terminate(_Reason, _State) ->
    error_logger:info_msg("Leaving  down_measure\n"),
    ok.

code_change(_OldVsn, State, Data ) ->
    {ok, State, Data}.

%Internal functions

receive_data (_Ref,N) ->
    receive
      {http, {RId, {error, Reason}}}      ->  error_logger:info_msg("Error [~p][~p][~p]\n",[RId,Reason,N]),
                                              N;
      {http, {RId, Result}}               ->  error_logger:info_msg("Result --[~p]-- [~p]\n",[RId,Result]),
                                              N;
      {http, {RId, stream_start, _Headers}}-> receive_data(RId,N);
      {http, {RId, stream, BodyPart}}     ->  B = N + byte_size(BodyPart),
                                              receive_data(RId,B);
      {http, {_RId, stream_end, _Headers}}  -> N
    end.
