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
%% Tries to check if a user can login.
%% @end
%% @param Multipart is the message to try to decode.
%% @return A tuple with error or a pair {User, Password} result of succesful {@link login_manager:login.
check_valid_login(Ident, Multipart) ->
    #{user := User, password := Password} = Multipart,
    case login_manager:login(User,Password) of
        {_, ok} ->
            ?CLIENTER_NAME ! {login, User, Ident},
            {User, Password};
        Err ->
            Err
    end.

just_logout(Multipart) ->
    #{user := User} = Multipart,
    login_manager:logout(User),
    logout.

            


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
    case handle_message(Ident, Multipart) of
        Login={User, _} -> 
            ?CLIENTER_NAME ! {login, User, Ident},
            handle_requests(Ident, Login, #{});
        M ->
            {error, M}
    end.

handle_login(Ident, Multipart, first) ->
    case handle_message(Ident, Multipart) of
        Login={User, _} -> 
            ?CLIENTER_NAME ! {login, User, Ident},
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
        {exchange, Name, PID} ->
            handle_requests(Ident, 
                            Login, 
                            map:put(Name, PID, Exchanges));
        Multipart ->
            case handle_multipart(Ident, Login, Multipart, Exchanges) of
                logout ->
                    ?CLIENTER_NAME ! {logout, User},
                    logout;
                _ ->
                    handle_requests(Ident, Login, Exchanges)
            end
    after ?STUPID_TIME_OUT ->
        ?CLIENTER_NAME ! {logout, User},
        ?CLIENTER_NAME ! {exit, Ident}
    end.

%% @doc
%% Handle requests for unauthenticated users. This amounts to checking for login again.
%% @end
handle_requests(Ident) ->
    receive
        Multipart -> 
            handle_login(Ident,Multipart),
            handle_requests(Ident)
    after ?TIME_OUT ->
        ?CLIENTER_NAME ! {exit, Ident}
    end.


handle_multipart(Ident, {User,_}, Multipart, Exchanges) ->
    case handle_message(Ident, Multipart) of
        logout ->
            ?CLIENTER_NAME ! {logout, User},
            logout;
        {ex_address, Ident, Name} ->
            case map:find(Name, Exchanges) of
                {ok, PID} ->
                    PID ! {ex_address, Ident, Name};
                error ->
                    ?EXCHANGER_NAME ! {ex_address, Ident, Name}
            end,
            nothing;
        {dir_address, Ident, M} ->
            ?CLIENTER_NAME ! {dir_address, Ident, M},
            nothing;
        M = {trade, Ident, Exchange, _} ->
            case map:find(Exchange, Exchanges) of
                {ok, PID} ->
                    PID ! M;
                error ->
                    ?EXCHANGER_NAME ! M
            end,
            nothing;
        _ ->
            error
    end.



handle_message(Ident, Multipart) ->
    case wrapper_client_pb:decode_msg(Multipart, 'WrapperMessageClient') of
        #{isLogin := true, message := M} ->
            check_valid_login(Ident, M);
        #{isLogout := true, message := M} ->
            just_logout(M);
        #{isAddressRequest := true, message := M} ->
            handle_address_request(Ident, M);
        #{isTrade := true, message := #{exchange := Exchange}} ->
            {trade, Ident, Exchange, Multipart}
    end.
        

handle_address_request(Ident, Address) ->
    #{name := Name,
      type := Type} = Address,
    case Type of
        'DIRECTORY' ->
                {Host, Port} = ?DIRECTORY,
                Adrs = #{host => Host, 
                         port => Port, 
                         name => <<"index.html">>, 
                         type => 'DIRECTORY'},               
                New = #{isAddress => true, message => Adrs},
                M = wrapper_client_pb:encode_msg(New),
                {dir_address, Ident, M};
        'EXCHANGE' ->
            {ex_address, Ident, Name}
    end.
