-module(exchanger_app).
-export([start/2, stop/1]).
-include("exchanger.hrl").

start(normal, _) ->
    application:ensure_started(chumak),
    process_flag(trap_exit, true),
    login_manager:start_with_link(),
    clienter_main:start_with_link(),
    exchanger_main:start_with_link(),
    ensure_running().

stop(State) ->
    ok.

ensure_running() ->
    receive 
        {'EXIT', ?CLIENTER_NAME, Reason} ->
            error_logger:error_msg("An error ocurred in clienter in ~p~n",Reason),
            exchanger_main:clear(),
            login_manager:clear(),
            ensure_running();
        {'EXIT', ?EXCHANGER_NAME, Reason} ->
            error_logger:error_msg("An error ocurred in exchanger in ~p~n",Reason),
            clienter_main:clear(),
            login_manager:clear(),
            ensure_running();
        {'EXIT', ?LOGINER_NAME, Reason} ->
            error_logger:error_msg("An error ocurred in clienter in ~p~n",Reason),
            clienter_main:clear(),
            exchanger_main:clear(),
            ensure_running()
    end.
