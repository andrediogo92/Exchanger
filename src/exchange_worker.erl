%% @doc 
%% Exchange module works out messages from main exchange connection demuxer
%% and forwards replies as needed back to exchange main demuxer or to user proxies.
%% @end
%% @see exchanger
%% @see user
-module(exchange_worker).
-export([start/2,start_with_link/2]).
-include("exchanger.hrl").

%% @doc
%% Start up an exchanger interaction process by going directly to handling requests.
%% @end
start(Ident, Multipart) ->
    spawn(fun() -> handle_online(Ident, Multipart, empty) end).

%% @doc
%% {@link start. Start with atomic link}.
start_with_link(Ident, Multipart) -> 
    spawn_link(fun() -> handle_online(Ident, Multipart, empty) end).


%% @doc
%% Handle requests incoming and encoded from the exchange demuxer.
%%
%% Used for authenticated users.
%% @end
handle_requests(Ident, Name, <<>>) ->
    ask_address(Ident, Name),
    receive
    after 300 ->
        dump
    end,
    receive
        {ex_address, Client, Name} ->
            Client ! failed_address;
        Multipart ->
            case handle_message(Ident, Multipart, Name) of
                exit ->
                    ?EXCHANGER_NAME ! {exit, Ident};
                {ok, Addr} ->
                    handle_requests(Ident,Name,Addr);
                _ ->
                    handle_requests(Ident,Name,<<>>)
            end
    after ?TIME_OUT ->
        ?EXCHANGER_NAME ! {exit, Ident}
    end;

handle_requests(Ident, Name, Cached) ->
    receive
        {ex_address, Client, Name} ->
            Client ! failed_address;
        Multipart ->
            case handle_message(Ident, Multipart, Name) of
                exit ->
                    ?EXCHANGER_NAME ! {exit, Ident};
                {ok, Addr} ->
                    handle_requests(Ident,Name,Addr);
                _ ->
                    handle_requests(Ident,Name,Cached)
            end
    after ?TIME_OUT ->
        ?EXCHANGER_NAME ! {exit, Ident}
    end.


%
%handle_message(Ident, Multipart, empty) ->
%    case wrapper_server:decode_msg(Multipart) of
%        #{isOnline := true, message := {_, M}} ->
%            handle_online(Ident, M, empty);
%        #{isAddress := true} ->
%            %dump
%            handle_requests(Ident);
%        #{isTradeCompleted := true} ->
%            %dump
%            handle_requests(Ident)
%    end.        


handle_message(Ident, Multipart, Exchange) ->
    case wrapper_server:decode_msg(Multipart) of
        #{isOnline := true, message := {_, M}} ->
            handle_online(Ident, M, Exchange);
        #{isAddress := true, message := {_, M}} ->
            handle_address(Ident, M, Multipart, Exchange);
        #{isTradeCompleted := true, message := {_, M}} ->
            handle_trade(M, Multipart,Exchange)
    end. 

handle_online(Ident, M, empty) ->
    case M of
        #{name := Exchange, status := 200} ->
            ?EXCHANGER_NAME ! {online, Ident, Exchange},
            handle_requests(Ident, Exchange, <<>>);
        #{status := 404} ->
            ?EXCHANGER_NAME ! {exit, Ident}
    end;

handle_online(Ident, M, Exchange) ->
    case M of
        #{name := Exchange, status := 200} ->
            ok;
        _ ->
            ?EXCHANGER_NAME ! {exit, Ident},
            exit
    end.

handle_address(Ident, M, Adr, Exchange) ->
    case M of
        #{name := Exchange} -> 
            {ok, Adr};
        _ ->
            ?EXCHANGER_NAME ! {exit, Ident},
            exit
    end.

ask_address(Ident, Exchange) ->
    T = #{isAddressRequest => true, message => {addressRequest, #{type => 'EXCHANGE', name => Exchange}}},
    M = wrapper_server_pb:encode_msg(T),
    ?EXCHANGER_NAME ! {address, Ident, M}.

handle_trade(M, Multipart, Exchange) ->
    case M of
        #{exchange := Exchange, user := User} ->
            ?CLIENTER_NAME ! {trade_completed, User, Multipart},
            ok;
        _ ->
            dump
    end.
