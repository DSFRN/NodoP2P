-module(nodo).
-export([start_server/0, server/1, listen/1]).
-export([start_client/0]).
-define(Puerto, 12345).
-define(Dir, "localhost").

start_server() ->
  {ok, Socket} = gen_tcp:listen(?Puerto, [{active, false}]),
  io:format("   SERVER IS UP!~n"),
  spawn(?MODULE, server, [Socket]).

server(Socket) ->
  case gen_tcp:accept(Socket) of
    {ok, CSocket}   ->
      spawn(?MODULE, listen, [CSocket]),
      server(Socket);
    {error, Reason} ->
      io:format("   SERVER_ERROR: ~p.~n", [Reason]),
      gen_tcp:close(Socket)
  end.

listen(CSocket) ->
  case gen_tcp:recv(CSocket, 0) of
    {ok, Packet}    -> 
      case Packet of
        "id_nodo" ->
          gen_tcp:send(CSocket, "Acá tenés tu id: ..."),
          listen(CSocket);
        "listar_mis_archivos" ->
          gen_tcp:send(CSocket, "Acá tenés tu lista: ..."),
          listen(CSocket);
        "salir" ->
          gen_tcp:send(CSocket, "Chau!~n"),
          gen_tcp:close(CSocket);
        _ ->
          gen_tcp:send(CSocket, "No ni idea la verdad"),
          listen(CSocket)
      end;

    {error, closed} ->
      io:format("   LISTEN_ERROR: conexión cerrada.\n"),
      gen_tcp:close(CSocket);
    {error, Reason} ->
      Error = "   LISTEN_ERROR: " ++ atom_to_list(Reason) ++ "\n",
      io:format(Error),
      gen_tcp:close(CSocket)
  end.

%-----------------------------------------------------------------------------------%

start_client() ->
  case gen_tcp:connect(?Dir, ?Puerto, [{active, false}]) of
    {ok, Socket} ->
      io:format("   CONNECTED | Enviar al server: "),
      Str = io:get_line(""),
      gen_tcp:send(Socket, string:trim(Str)),
      case gen_tcp:recv(Socket, 0) of
        {ok, Paquete}   -> io:format("   RECV -> ~p~n", [Paquete]);
        {error, Reason} -> io:format("   RECV_ERROR: ~p.~n", [Reason])
      end,
      gen_tcp:close(Socket),
      ok_closed;

    {error, Reason} -> io:format("   CONNECT_ERROR: ~p.~n", [Reason])
  end.
