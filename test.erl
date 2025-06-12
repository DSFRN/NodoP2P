-module(test).
-export([main/0]).

main() ->
  [ID, Host] = string:split(io_lib:format("~s",[node()]), "@"),
  case ID of
    "nonode" ->
      io:format("ERROR: No ID was especified.~n");
    _node_id ->
      io:format("~p~n", [ID]),
      case inet:getaddr(Host, inet) of
        {ok, Addr} ->
          io:format("~p~n", [Addr]);
        {error, _Reason} ->
          io:format("ADDRESS ERROR~n")
      end
  end,
  ok.
