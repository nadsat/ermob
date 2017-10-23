%%%-------------------------------------------------------------------
%% @doc vutils public API
%% @end
%%%-------------------------------------------------------------------

-module(vutils).


%% Application callbacks
-export([get_ipv4/1,get_date/0,get_devid/0]).
%%====================================================================
%% API
%%====================================================================

get_ipv4 (IFace)->
    InfoList = case inet:getifaddrs() of
      {ok, Iflist} ->  proplists:get_value(IFace,Iflist);
      _B           ->  undefined
    end,
    IPv4List = case InfoList of
      undefined -> [];
      L         -> AList = lists:filter (fun({X,_Y})-> (X == addr) end,L),
                   lists:filter (fun({addr,Y})-> tuple_size(Y)==4 end,AList)
    end,
    case IPv4List of
      []     -> "0.0.0.0";
      [H|_T] -> {addr,IP} = H,
                inet:ntoa(IP)
    end.

get_date()->
    {{Year,Month,Day},{HH,MM,SS}} = calendar:local_time(),
    io_lib:format('~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b', [Year,Month,Day,HH,MM,SS]).

get_devid() ->
  {ok,Name} = inet:gethostname(),
  Name.
%%====================================================================
%% Internal functions
%%====================================================================
