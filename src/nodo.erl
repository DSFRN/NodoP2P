-module(nodo).
-export([start_server/0, server/1, listen/1, manager/2]).
-export([client/1, connect/0, shared/0, get_info/0]).
-define(Port, 12345).
-define(Dir, "localhost").


start_server() ->
  {ok, Socket} = gen_tcp:listen(?Port, [{packet, 0}, {active, false}, {reuseaddr, true}]),
  io:format(" SERVER IS UP!~n"),
  server(Socket).


server(Socket) ->
  case gen_tcp:accept(Socket) of
    {ok, CSocket}   ->
      spawn(?MODULE, listen, [CSocket]),
      nodo:server(Socket);
    {error, Reason} ->
      io:format(" SERVER_ERROR: ~p.~n", [Reason]),
      gen_tcp:close(Socket)
  end.


listen(CSocket) ->
  case gen_tcp:recv(CSocket, 0) of
    {ok, Packet} -> 
      case Packet of
        "salir" ->
          gen_tcp:send(CSocket, "fin"),
          gen_tcp:close(CSocket);

        _ ->
          spawn(?MODULE, manager, [Packet, CSocket]),
          nodo:listen(CSocket)
      end;

    {error, closed} ->
      io:format("\n LISTEN_ERROR: conexión cerrada.\n"),
      gen_tcp:close(CSocket);

    {error, Reason} ->
      Error = "\n LISTEN_ERROR: " ++ atom_to_list(Reason) ++ "\n",
      io:format(Error),
      gen_tcp:close(CSocket)
  end.


manager(Packet, CSocket) ->
  case Packet of

    "id_nodo" ->
      case nodo:get_info() of
        {ID, Addr} ->
           Info = io_lib:format("ID: ~s - IP: ~p~n", [ID, Addr]),
           gen_tcp:send(CSocket, Info);
        Reason ->
           gen_tcp:send(CSocket, Reason)
      end;

    "listar_archivos" ->
      case nodo:shared() of
        {ok, Filenames} ->
          Lista = lists:foldl(fun(File, Acc) -> File ++ "\n " ++ Acc end, "", Filenames),
          gen_tcp:send(CSocket, Lista);
        {error, Reason} ->
          gen_tcp:send(CSocket, Reason)
      end;

     _ -> gen_tcp:send(CSocket, "No te entiendo, trata de vuelta.\n")

  end.


shared() ->
  file:list_dir_all("./shared").


get_info() ->
  [ID, Host] = string:split(io_lib:format("~s",[node()]), "@"),
  case ID of
    "nonode" ->
      Reason = " ERROR: No ID was especified.\n",
      Reason;
    _ ->
      case inet:getaddr(Host, inet) of
        {ok, Addr} -> {ID, Addr};
        {error, Reason} -> Reason
      end
  end.

%-----------------------------------------------------------------------------------%

connect() ->
  case gen_tcp:connect(?Dir, ?Port, [{active, false}]) of
    {ok, CSocket} ->
      client(CSocket);
    {error, Reason} ->
      io:format(" CONNECT_ERROR: ~p.~n", [Reason])
  end.


client(CSocket) ->
  io:format(" CONNECTED | Enviar al server: "),
  Str = io:get_line(""),
  gen_tcp:send(CSocket, string:trim(Str)),

  case gen_tcp:recv(CSocket, 0) of
    {ok, "fin"} ->
       io:format(" DISCONNECTING...~n"),
       gen_tcp:close(CSocket);

    {ok, Paquete} ->
       io:format(" RECV:~n ~s~n", [Paquete]),
       nodo:client(CSocket);

    {error, Reason} ->
       io:format(" RECV_ERROR: ~p.~n", [Reason]),
       gen_tcp:close(CSocket)
  end.

