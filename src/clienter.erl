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
            handle_cli_messages(Rec, CSockets, #{})
    end.

handle_cli_messages(Rec, CSockets={CSocket,_}, Known) ->
    receive
        {trade_completed, User, Trade} ->
            Ident = maps:get(User,Known),
            chumak:send_multipart(CSocket,[Ident, Trade]),
            handle_cli_messages(Rec, CSockets, Known);
        {ex_address, Ident, M} ->
            chumak:send_multipart(CSocket,[Ident, M]),
            handle_cli_messages(Rec, CSockets, Known);
        {dir_address, Ident, M} ->
            chumak:send_multipart(CSocket,[Ident, M]),
            handle_cli_messages(Rec, CSockets, Known);
        {logout, User} ->
            New = maps:remove(User, Known),
            handle_cli_messages(Rec,CSockets, New);
        {recv, Ident, Multipart} -> 
            New = handle_message(Ident, Known, Multipart),
            handle_cli_messages(Rec, CSockets, New);
        {exit, Ident} ->
            New = maps:remove(Ident,Known),
            handle_cli_messages(Rec, CSockets, New);
        {login, User, Ident} ->
            New = maps:put(User, Ident, Known),
            Login=login:encode_msg(#{code => 200},'login_reply'),
            chumak:send_multipart(CSocket,[Ident, Login]),
            handle_cli_messages(Rec, CSockets, New)
    end.

handle_message(Ident, Known, Multipart) ->
    B = maps:is_key(Ident,Known),
    case B of
        true ->
            maps:get(Ident,Known) ! Multipart,
            Known;
        false ->
            PID = user:start(Ident, Multipart),
            maps:put(Ident, PID, Known)
    end.




