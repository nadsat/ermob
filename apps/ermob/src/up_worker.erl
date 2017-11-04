-module(up_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2]).
-record(state,{c_timeout,
               u_timeout}).
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    Con_timeout  = proplists:get_value(con_timeout, Args),
    Up_timeout = proplists:get_value(up_timeout, Args),
    State = #state{c_timeout=Con_timeout,
                   u_timeout=Up_timeout},
    {ok, State,hibernate}.

handle_info(_Msg, State) ->
    {noreply, State,hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State,hibernate}.

handle_call({up,Url,Filesize,Pid}, _From,State)  ->
    #state{c_timeout=Con_timeout,
	         u_timeout=Up_timeout} = State,
    Op = [{timeout,Up_timeout},{connect_timeout,Con_timeout}],
    %stream upload
    SelfPID = self(),
    LenPerTick = 12800,
    DummyPayload = "content0="++lists:duplicate(LenPerTick-9, "1"),
    BodyFun = 
	fun(0) ->  	eof;
    	(LenLeft)-> 	Bytes = Filesize - LenLeft,
			SelfPID ! {bodyfun, set_progress, Bytes},
			{ok, DummyPayload, LenLeft - LenPerTick}
    	end,
    %async test
    {ok,Ref} =
     	httpc:request(post, {Url, [ {"content-length", integer_to_list(Filesize)} ], 
		"application/x-www-form-urlencoded", {BodyFun, Filesize}}, Op, [{sync,false},{stream,self}] ),
    Bytes = receive_data(Ref,0),
    Pid ! {done,Bytes},
    {reply, Bytes, State, hibernate}.

terminate(_Reason, _State) ->
    error_logger:info_msg("Leaving up_measure\n"),
    ok.

code_change(_OldVsn, State, _Data) ->
    {ok, State}.


%Internal functions

receive_data (_Ref,N) ->
    receive
      {bodyfun, set_progress, B}	          -> receive_data(fakeref123,B);
      {http, {RId, {error, Reason}}}        -> error_logger:info_msg("Error [~p][~p][~p]\n",[RId,Reason,N]),
                                               N;
      {http, {_RId, _Result}}               -> N;
      {http, {RId, stream_start, _Headers}} -> receive_data(RId,N);
      {http, {RId, stream, BodyPart}}       -> {Bytes, _Rest} = string:to_integer(string:substr( binary_to_list(BodyPart) ,6)),
					                                     receive_data(RId,Bytes);
      {http, {_RId, stream_end, _Headers}}   -> N
    end.

