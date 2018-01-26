-module(exchanger_main).
-export([start/0, start_with_link/0]).
-include("exchanger.hrl").

start_with_link() -> register(?EXCHANGER_NAME, spawn_link(fun ()-> server() end)).

start() -> register(?EXCHANGER_NAME, spawn(fun ()-> server() end)).


create_and_bind_exchanges() ->
    {ok,ESocket} = chumak:socket(router ),
    {ok,_EBindPid} = chumak:bind(ESocket,tcp,"localhost",?PORT_EXCHANGE),
    {ESocket,_EBindPid}.

setup() ->
    ESockets = create_and_bind_exchanges(),
    ?CLIENTER_NAME ! ESockets,
    loop(ESockets).
        
loop({ESocket,Bind}) ->
    {ok, [Identity | Part]} = chumak:recv_multipart(ESocket),
    ?CLIENTER_NAME ! {recv, Identity, Part},
    loop({ESocket,Bind}).
        

server() -> 
    process_flag(trap_exit, true),
    Rec = spawn_link(fun() -> setup() end),
    receive
        ESockets ->
            handle_ex_messages(Rec, ESockets, #{})
    end.

handle_ex_messages(Rec, ESockets={ESocket,_}, Known) ->
    receive
        {address, Ident, M} ->
            chumak:send_multipart(ESocket,[Ident,M]),
            handle_ex_messages(Rec,ESockets,Known);
        {trade_request, Client, Exchange, Trade} ->
            case maps:find(Exchange,Known) of
                {ok, Ident} ->
                    chumak:send_multipart(ESocket,[Ident, Trade]),
                    handle_ex_messages(Rec, ESockets, Known);
                error ->
                    Client ! failed,
                    handle_ex_messages(Rec, ESockets, Known)
            end;
        {ex_address, Client, Name} ->
            handle_find(Known, Client, Name),
            handle_ex_messages(Rec, ESockets, Known);
        {recv, Ident, Multipart} -> 
            New = handle_message(Ident, Known, Multipart),
            handle_ex_messages(Rec, ESockets, New);
        {exit, Ident} ->
            New = maps:remove(Ident,Known),
            handle_ex_messages(Rec, ESockets, New)
    end.

handle_message(Ident, Known, Multipart) ->
    B = maps:is_key(Ident,Known),
    case B of
        true ->
            maps:get(Ident,Known) ! Multipart,
            Known;
        false ->
            PID = exchange_worker:start(Ident, Multipart),
            maps:put(Ident, PID, Known)
    end.

handle_find(Known, Client, Name) ->
    case maps:find(Name,Known) of
        {ok, Ident} ->
            case maps:find(Ident, Known) of 
                {ok, PID} ->
                    PID ! {ex_address, Client, Name};
                error ->
                    Client ! failed_address
            end;
        error ->
            Client ! failed_address
    end.
