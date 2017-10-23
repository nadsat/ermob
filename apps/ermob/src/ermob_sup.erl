%%%-------------------------------------------------------------------
%% @doc ermob top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ermob_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Loop = [{ermob_loop,
            {ermob_loop, start_link, []},
            permanent,
            60000,
            worker,
            [vime_loop]}],
    {ok, TList} = application:get_env(tests),
    L     = lists:map(fun ({Name,_,_})->
                        Name
                      end,TList),
    ASet  = sets:from_list(L),
    AList = sets:to_list(ASet),
    ProcList = lists:map(fun (Tname)->
                        {Tname,
                         {Tname, start_link, []},
                         permanent,
                         60000,
                         worker,
                         [Tname]}
                      end,AList),
    {ok, Pools} = application:get_env(pools),
    PoolList    = lists:map(fun ({Name,Conf})->
                        SizeArgs   = proplists:get_value(size_args, Conf),
                        WorkerArgs = proplists:get_value(worker, Conf ),
                        WorkerMod  = proplists:get_value(worker_module, Conf ),
                        PoolArgs = [{name, {local,Name }},
                                    {worker_module, WorkerMod}] ++ SizeArgs,
                        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
                      end,Pools),
    Proc = lists:append([ProcList, PoolList,Loop]),
    MaxRestart = 5,
    MaxTime = 10000,
    {ok, {{one_for_one, MaxRestart, MaxTime},
          Proc}}.

%%====================================================================
%% Internal functions
%%====================================================================
