-module(clienter).
-export([start/0, start_with_link/0]).
-include("main.hrl").


start() -> register(?CLIENTER_NAME, spawn(fun() -> server() end)).

start_with_link() -> register(?CLIENTER_NAME, spawn_link(fun() -> server() end)).


create_and_bind_clients() ->
    {ok,CSocket} = chumak:socket(router),
    {ok,_CBindPid} = chumak:bind(CSocket,tcp,"localhost",?PORT_CLIENTS),
    {CSocket,_CBindPid}.

server() -> 
    CSockets = create_and_bind_clients(),    
    handle_messages(CSockets, []).


handle_messages(CSockets, Known) ->
    receive
        {recv, Ident, Multipart} -> 
            New = handle_message(Ident, Known, Multipart),
            handle_messages(CSockets, New);
        {closed, Ident} ->
            New = maps:remove(Ident,Known),
            handle_messages(CSockets, New)
    end.

handle_message(Ident, Known, Multipart) ->
    B = maps:is_key(Ident,Known),
    case B of
        true ->
            maps:get(Ident,Known) ! Multipart,
            Known;
        false ->
            PID = spawn(main,user_run,[Ident,Multipart]),
            maps:put(Known, Ident, PID)
    end.


user_run(Ident, Multipart) ->
    case check_valid_login(Clients, Ident, Multipart) of
                {ok, Login} ->    
    receive
        Bin when is_map(login:decode_msg(Multipart,'Login')) ->

    end.

check_valid_login(Ident, Multipart) ->

