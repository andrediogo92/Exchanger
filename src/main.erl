-module(main).
-export([main/0]).
-include("main.hrl").

main() ->
    application:ensure_started(chumak),
    process_flag(trap_exit, true),
    login_manager:start_with_link(),
    clienter:start_with_link(),
    exchanger:start_with_link(),
    ensure_running().

ensure_running() ->
    receive 
        {'EXIT', ?CLIENTER_NAME, Reason} ->
            error_logger:error_msg("An error ocurred in clienter in ~p~n",Reason),
            exchanger:clear(),
            login_manager:clear(),
            ensure_running();
        {'EXIT', ?EXCHANGER_NAME, Reason} ->
            error_logger:error_msg("An error ocurred in exchanger in ~p~n",Reason),
            clienter:clear(),
            login_manager:clear(),
            ensure_running();
        {'EXIT', ?LOGINER_NAME, Reason} ->
            error_logger:error_msg("An error ocurred in clienter in ~p~n",Reason),
            clienter:clear(),
            exchanger:clear(),
            ensure_running()
    end.
