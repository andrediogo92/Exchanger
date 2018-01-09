-module(exchanger).
-export([start/0, start_with_link/0]).
-include("main.hrl").

start_with_link() -> register(?EXCHANGER_NAME, spawn_link(fun ()-> server() end)).

start() -> register(?EXCHANGER_NAME, spawn(fun ()-> server() end)).


create_and_bind_exchanges() ->
    {ok,ESocket} = chumak:socket(router ),
    {ok,_EBindPid} = chumak:bind(ESocket,tcp,"localhost",?PORT_EXCHANGE),
    {ESocket,_EBindPid}.


server() ->
    ESockets = create_and_bind_exchanges(),
    handle_ex_messages(ESockets,[]).

handle_ex_messages(ESockets,[]) ->
    ESockets.

