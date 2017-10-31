%%%-------------------------------------------------------------------
%% @doc vutils public API
%% @end
%%%-------------------------------------------------------------------

-module(serial_utils).


%% Application callbacks
-export([split_cmd/1,split_response/1]).
%%====================================================================
%% API
%%====================================================================

split_cmd (Cmd)->
  Len = length(Cmd),
  TmpCmd = string:sub_string(Cmd,3,Len-1),  %% get rid of "AT" part
  TmpSplit = binary:split(list_to_binary(TmpCmd),<<"=">>),
  split_at_r(TmpSplit).

split_response (Cmd)->
  TmpSplit = binary:split(Cmd,<<":">>),
  split_at_r(TmpSplit).

%% Returns tuple {atom,Payload} where Payload is the right part after "=" or ":"
split_at_r([Cmd,<<>>]) ->
  split_at_r([Cmd]);
split_at_r([Cmd,Rest]) ->
  B = remove_fc(Cmd),
  {cmd_to_atom(B),Rest};
split_at_r([Cmd]) ->
  B = remove_fc(Cmd),
  {cmd_to_atom(B),<<>>}.

%% Returns atom either from command part or string before of ":" from response
cmd_to_atom(Cmd)->
  Str = binary_to_list(Cmd),
  Pos = string:chr(Str,$?),
  TmpStr = case Pos > 0 of
    true  -> string:sub_string(Str,1,Pos-1);
    false -> Str
  end,
  NoSpace = replace_space (TmpStr),
  list_to_atom(string:to_lower(NoSpace)).

%% Remove first character when either is + or ^
remove_fc ( << H:8,R/binary >> ) when H == 43 ->
  R;
remove_fc ( << H:8,R/binary >> ) when H == 94 ->
  R;
remove_fc ( << H:8,R/binary >> )  ->
  << H:8,R/binary >> .

replace_space (Str)->
  [case X of $ ->$_;_->X end||X<-Str ].

%%====================================================================
%% Internal functions
%%====================================================================
