%% @doc 
%% Exchange module works out messages from main exchange connection demuxer
%% and forwards replies as needed back to exchange main demuxer or to user proxies.
%% @end
%% @see exchanger
%% @see user
-module(exchange).
-export([start/2,start_with_link/2]).
-include("main.hrl").

%% @doc
%% Start up an exchanger interaction process by going directly to handling requests.
%% @end
start(Ident, Multipart) ->
    spawn(fun() -> handle_message(Ident, Multipart, <<>>, first) end).

%% @doc
%% {@link start. Start with atomic link}.
start_with_link(Ident, Multipart) -> 
    spawn_link(fun() -> handle_message(Ident, Multipart, <<>>, first) end).


check_heartbeat(Multipart) ->
    try online_pb:decode_msg(Multipart,'online') of
        Online ->
            #{name := Name,
              status := Status} = Online,
            {Name, Status}
    catch
        error:Err ->
            {error, Err}
    end.

check_trade_complete(Multipart) ->
    try trade_pb:decode_msg(Multipart,'trade_completed') of
        Trade ->
            #{exchange := Exchange,
              user := User} = Trade,
            {trade_completed, Exchange, User, Multipart}
    catch
        error:Err ->
            {error, Err}
    end.

%% @doc
%% Handle requests incoming and encoded from the exchange demuxer.
%%
%% Used for authenticated users.
%% @end
handle_requests(Ident, Name) ->
    receive
        Multipart ->
            handle_multipart(Ident, Multipart, Name),
            handle_requests(Ident, Name);
        {T, Ident, Trade} ->

    after ?TIME_OUT ->
        ?EXCHANGER_NAME ! {exit, Ident}
    end.

handle_multipart(Ident, Multipart, Name) ->
    case handle_message(Multipart) of
        {ok, Name} ->
            ok;
        {ok, Exchange} ->
            {invalid, Exchange};
        {dead, Exchange} ->
            {invalid, Exchange};
        {trade_completed, Name} ->
            ok;
        {trade_completed, Exchange} ->
            {invalid, Exchange}
    end.



handle_message(Ident, Multipart, <<>>, first) ->
    case check_heartbeat(Multipart) of
        {Exchange, 200} ->
            ?EXCHANGER_NAME ! {online, Ident, Exchange},
            handle_requests(Ident, Exchange);
        {_, 404} ->
            ?EXCHANGER_NAME ! {exit, Ident};
        {error, _} ->
            case handle_trade(Multipart) of
                {trade_completed, Exchange} ->
                    ?EXCHANGER_NAME ! {online, Ident, Exchange},
                    handle_requests(Ident, Exchange);
                _ ->
                    handle_requests(Ident, <<>>)
            end
    end.        

handle_message(Multipart) ->
    case check_heartbeat(Multipart) of
        {Exchange, 200} ->
            {ok, Exchange};
        {Exchange, 404} ->
            {dead, Exchange};
        {error, _} ->
            handle_trade(Multipart)
    end.

handle_trade(Multipart) ->
    case check_trade_complete(Multipart) of
        {trade_completed, Exchange, User, Multipart} ->
            ?CLIENTER_NAME ! {trade_completed, User, Multipart},
            {trade_completed, Exchange};
        E={error, _} ->
            E
    end.
