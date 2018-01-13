%% @doc 
%% User module works out messages from main client connection demuxer
%% and forwards as needed to exchange main demuxer but directly to
%% exhange proxies after discovery.
%% @end
%% @see clienter
%% @see exchange
-module(user).
-export([start/2,start_with_link/2]).
-include("main.hrl").

%% @doc
%% Start up a user interaction process by going to login handling.
%% @end
start(Ident, Multipart) ->
    spawn(fun() -> handle_login(Ident, Multipart, first) end).

%% @doc
%% {@link start. Start with atomic link}.
start_with_link(Ident, Multipart) -> 
    spawn_link(fun() -> handle_login(Ident, Multipart, first) end).



%% @doc
%% Tries to check if a message can be decoded as a {@link login_pb:login_request. login request}.
%%
%% Decoding failure results in error.
%% @end
%% @param Multipart is the message to try to decode.
%% @return A tuple with error or the result of {@link login_manager:login, attempting to login in the login_manager}.
check_valid_login(Multipart) ->
    try login:decode_msg(Multipart,login_request) of
        Login ->
            #{user := User, password := Password} = Login,
            {login_manager:login(User,Password),{User,Password}}
    catch    
        error:Err ->
            {error, Err}
    end.


check_trade_request(Ident, {Logged, _}, Multipart) ->
    try trade_pb:decode_msg(Multipart,'trade_order') of
        Trade ->
            #{buy_or_sell := Order,
              exchange := Exchange,
              company := Company, 
              quant := Quant,
              price := Price,
              user := User} = Trade,
            case Logged=:=User of
                true ->
                    case Order of
                        true -> 
                            {trade_buy, Ident, {Exchange, Company, Quant, Price, User}};
                        false ->
                            {trade_sell, Ident, {Exchange, Company, Quant, Price, User}}
                    end;
                false ->
                    login_manager:logout(Logged),
                    {logout,Logged}
            end
    catch
        error:Err ->
            {error, Err}
    end.


check_address_request(Ident, Multipart) ->
    try trade_pb:decode_msg(Multipart,'address_request') of
        Address ->
            #{name := Name,
              type := Type} = Address,
            case Type of
                'DIRECTORY' ->
                    {dir_address, Ident, Name};
                'EXCHANGE' ->
                    {ex_address, Ident, Name}
            end
    catch
        error:Err ->
            {error, Err}
    end.
            


%% @doc
%% A client must login before anyting else.
%%
%% On fail goes back to handling requests to login exclusively.
%%
%% On success goes to handling exchange or directory requests.
%% @end
%% @param Ident is the ZeroMQ identity so client demuxer knows who to send messages to.
%% @param Multipart is the message that may contain a {@link login_pb:login_request. login request}.
handle_login(Ident, Multipart) ->
    case check_valid_login(Multipart) of
        {ok, Login} -> 
            ?CLIENTER_NAME ! {login, Ident},
            handle_requests(Ident, Login, #{});
        M ->
            {error, M}
    end.

handle_login(Ident, Multipart, first) ->
    case check_valid_login(Multipart) of
        {ok, Login} -> 
            ?CLIENTER_NAME ! {login, Ident},
            case handle_requests(Ident, Login, #{}) of
                logout ->
                    handle_requests(Ident);
                exit ->
                    exit
            end;
        _ ->
            handle_requests(Ident)
    end.

%% @doc
%% Handle requests incoming and encoded from the client demuxer.
%%
%% Used for authenticated users.
%% @end
handle_requests(Ident, Login={User,_}, Exchanges) ->
    receive
        {logout, User} ->
            logout;
        {exchange, Name, PID} ->
            handle_requests(Ident, 
                            Login, 
                            map:put(Name, PID, Exchanges));
        {trade_completed, {Company, Quant, Total}} ->
            ?CLIENTER_NAME ! {trade_completed, Ident, {Company, Quant, Total}},
            handle_requests(Ident, Login, Exchanges);
        Multipart ->
            handle_multipart(Ident, Login, Multipart, Exchanges),
            handle_requests(Ident, Login, Exchanges)
    after ?STUPID_TIME_OUT ->
        ?CLIENTER_NAME ! {exit, Ident}
    end.

%% @doc
%% Handle requests for unauthenticated users. This amounts to checking for login again.
%% @end
handle_requests(Ident) ->
    receive
        Multipart -> 
            case handle_login(Ident,Multipart) of
                logout ->    
                    handle_requests(Ident);
                exit ->
                    exit
        end
    after ?TIME_OUT ->
        ?CLIENTER_NAME ! {exit, Ident}
    end.


handle_multipart(Ident, Login, Multipart, Exchanges) ->
    case handle_message(Ident, Login, Multipart) of
        {error, _} ->
            error;
        {ex_address, Ident, Name} ->
            case map:find(Name, Exchanges) of
                {ok, PID} ->
                    PID ! {ex_address, Ident, Name};
                error ->
                    ?EXCHANGER_NAME ! {ex_address, Ident, Name}
            end;
        {T, Ident, Trade={Exchange,_,_,_,_}} ->
            case map:find(Exchange, Exchanges) of
                {ok, PID} ->
                    PID ! {T, Ident, Trade};
                error ->
                    ?EXCHANGER_NAME ! {T, Ident, Trade}
            end;
        M ->
            ?CLIENTER_NAME ! M
    end.



handle_message(Ident, Login, Multipart) ->
    case check_trade_request(Ident, Login, Multipart) of
        L={_, _, _} ->
            L;
        {error, _} ->
            handle_address_request(Ident, Multipart)
    end.
        
handle_address_request(Ident, Multipart) ->
    case check_address_request(Ident, Multipart) of
        L={_, _, _} ->
            L;
        {error, M} ->
            {error, M}
    end.
