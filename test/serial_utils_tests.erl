-module(serial_utils_tests).
-include_lib("eunit/include/eunit.hrl").

split_cmd_test_()->
  Tests = [
            { "AT+CREG?",{creg,<<>>}},
            { "AT+CREG=2",{creg,<<>>}},
            { "AT^HCSQ?",{hcsq,<<>>}}
          ],
  [{V, ?_assertEqual(R, serial_utils:split_cmd(V))} || {V, R} <- Tests].


split_response_test_()->
  Tests = [
            { <<"^HCSQ: \"WCDMA\",43,36,51">>,{hcsq,<<" \"WCDMA\",43,36,51">>}},
            { <<"+CREG: 0,3">>,{creg,<<" 0,3">>}},
            { <<"^USSDMODE: 1">>,{ussdmode,<<" 1">>}}
          ],
  [{V, ?_assertEqual(R, serial_utils:split_response(V))} || {V, R} <- Tests].
