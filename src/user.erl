-module(user).
-export([start/2,start_with_link/2]).
-include("main.hrl").

start(Ident, Multipart) ->
    spawn(fun() -> handle_login(Ident, Multipart) end).

start_with_link(Ident, Multipart) -> 
    spawn_link(fun() -> handle_login(Ident, Multipart) end).

handle_login(Ident, Multipart) ->
    case check_valid_login(Multipart) of
        {ok, Login} -> 
            ?CLIENTER_NAME ! {login, Ident},
            handle_requests(Ident, Login);
        {invalid, _} ->
            handle_requests(Ident);
        {error, _} ->
            handle_requests(Ident)
    end.

check_valid_login(Multipart) ->
    try login:decode_msg(Multipart,login_request) of
        Login ->
            #{user := User, password := Password} = Login,
            {login_manager:login(User,Password),{User,Password}}
    catch    
        error:Err ->
            {error, Err}
    end.
    
handle_requests(Ident) ->
    receive
        Multipart -> 
            handle_login(Ident,Multipart)
    after ?TIME_OUT ->
        ?CLIENTER_NAME ! {exit, Ident}
    end.

handle_requests(Ident, Login) ->
    receive
        Multipart ->
            handle_message(Multipart, Login)
    after ?STUPID_TIME_OUT ->
        ?CLIENTER_NAME ! {exit, Ident}
    end.

handle_message(Multipart, Login) ->
    case check_trade(Multipart) of
        {ok, Trade} ->
            %placeholder
            [Trade, Login]
    end.

check_trade(Multipart) ->
%placeholder
    Multipart.
