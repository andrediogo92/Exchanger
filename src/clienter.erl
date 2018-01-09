-module(clienter).
-export([start/0, start_with_link/0]).
-include("main.hrl").


start() -> register(?CLIENTER_NAME, spawn(fun() -> server() end)).

start_with_link() -> register(?CLIENTER_NAME, spawn_link(fun() -> server() end)).


create_and_bind_clients() ->
    {ok,CSocket} = chumak:socket(router),
    {ok,_CBindPid} = chumak:bind(CSocket,tcp,"localhost",?PORT_CLIENTS),
    {CSocket,_CBindPid}.

setup() ->
    CSockets = create_and_bind_clients(),
    ?CLIENTER_NAME ! CSockets,
    loop(CSockets).

loop({CSocket,Bind}) ->
    {ok, [Identity | Part]} = chumak:recv_multipart(CSocket),
    ?CLIENTER_NAME ! {recv, Identity, Part},
    loop({CSocket,Bind}).

server() -> 
    process_flag(trap_exit, true),
    Rec = spawn_link(fun() -> setup() end),
    receive
        CSockets ->
            handle_cli_messages(Rec, CSockets, [])
    end.

handle_cli_messages(Rec, CSockets, Known) ->
    receive
        {recv, Ident, Multipart} -> 
            New = handle_message(Ident, Known, Multipart),
            handle_cli_messages(Rec, CSockets, New);
        {exit, Ident} ->
            New = maps:remove(Ident,Known),
            handle_cli_messages(Rec, CSockets, New);
        {login, ok, Ident} ->
            {CSocket,_} = CSockets,
            chumak:send_multipart(CSocket,[Ident,login:encode_msg(#{code => 200},'login_reply')]),
            handle_cli_messages(Rec, CSockets, Known)
    end.

handle_message(Ident, Known, Multipart) ->
    B = maps:is_key(Ident,Known),
    case B of
        true ->
            maps:get(Ident,Known) ! Multipart,
            Known;
        false ->
            PID = user:start(Ident, Multipart),
            maps:put(Known, Ident, PID)
    end.




