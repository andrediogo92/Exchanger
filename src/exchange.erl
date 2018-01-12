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
    spawn(fun() -> handle_message(Ident, Multipart) end).

%% @doc
%% {@link start. Start with atomic link}.
start_with_link(Ident, Multipart) -> 
    spawn_link(fun() -> handle_message(Ident, Multipart) end).

%% @doc
%% Handle requests incoming and encoded from the exchange demuxer.
%%
%% Used for authenticated users.
%% @end
handle_requests(Ident) ->
    receive
        Multipart ->
            handle_message(Ident, Multipart)
    after ?STUPID_TIME_OUT ->
        ?EXCHANGER_NAME ! {exit, Ident}
    end.


handle_message(Ident, Multipart) ->
    case check_heartbeat(Multipart) of
        ok ->
            ok;
        {error, _} ->
            handle_trade(Ident, Multipart)
    end.

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
        