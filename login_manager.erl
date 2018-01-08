-module(login_manager).
-export([start_with_link/0,start/0, login/2, logout/1, online/0]).
-include("main.hrl").

start() -> register(?LOGINER_NAME,spawn(fun() -> loop(?KNOWN)end)).

start_with_link() -> register(?LOGINER_NAME,spawn_link(fun() -> loop(?KNOWN)end)).

loop(Dick) -> 
    receive
        {{login,U,P},From} -> 
            case maps:find(U,Dick) of 
                {ok, {P,false}} -> 
                    From ! {?MODULE,ok},
                    loop(maps:update(U,{P,true},Dick));

                _ -> From ! {?MODULE,invalid},
                     loop(Dick)
            end;
        {{logout,U},From} -> 
            case maps:find(U,Dick) of
                {ok, {P,true}} -> 
                    From ! {?MODULE,ok},
                    loop(maps:update(U,{P,false},Dick));

                _ -> From ! {?MODULE,invalid},
                     loop(Dick)
            end;
        {online,From} ->
            R = [U || {U,{_,true}} <- maps:to_list(Dick)],
%           legacy v--v
%           M = maps:to_list(Dick),
%           F = lists:filter(M,fun(_,{_,B}) -> B == true end),
%           R = lists:map(F,fun({U,{_,true}}) -> U end),
            From ! {?MODULE,R},
            loop(Dick)
    end.

sendReceive(A) -> 
    ?MODULE ! {A,self()},
    receive {?MODULE,R} -> R end.

login(U,P) -> 
    sendReceive({login,U,P}).

logout(U) -> 
    sendReceive({logout,U}). 

online() ->
    sendReceive(online).
