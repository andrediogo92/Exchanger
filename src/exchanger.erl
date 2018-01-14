-module(exchanger).
-export([start/0, start_with_link/0]).
-include("main.hrl").

start_with_link() -> register(?EXCHANGER_NAME, spawn_link(fun ()-> server() end)).

start() -> register(?EXCHANGER_NAME, spawn(fun ()-> server() end)).


create_and_bind_exchanges() ->
    {ok,ESocket} = chumak:socket(router ),
    {ok,_EBindPid} = chumak:bind(ESocket,tcp,"localhost",?PORT_EXCHANGE),
    {ESocket,_EBindPid}.

setup() ->
    CSockets = create_and_bind_exchanges(),
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
        ESockets ->
            handle_ex_messages(Rec, ESockets, #{})
    end.

handle_ex_messages(Rec, ESockets={ESocket,_}, Known) ->
    ESockets.

