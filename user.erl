-module(user).
-export([start/2,start_with_link/2]).
-include("main.hrl").

start(Ident, Multipart) ->
    spawn(fun() -> user_run(?NUM_RETRIES, Ident, Multipart) end).

start_with_link(Ident, Multipart) -> 
    spawn_link(fun() -> user_run(?NUM_RETRIES, Ident, Multipart) end).

user_run(N, Ident, Multipart) ->
    case check_valid_login(Ident, Multipart) of
        {ok, Login} -> 
            handle_requests(Login);
        {invalid, Login} ->
            {N,Login}
    end.

check_valid_login(Ident, Multipart) ->
    Login = login:decode_msg(Multipart,'Login'),
    B = is_map(Login),
    case B of
        true ->
            {User,Password} = hd(maps:to_list(Login)),
            {login_manager:login(User,Password),Login};
        false ->
            {Ident, todo}
    end.
    
handle_requests(Login) ->
    Login.
