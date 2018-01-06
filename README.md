# Exchanger #

Erlang financial exchange front-end.

## Purpose ##

Is in charge of client authentication and redirecting buy and sell orders to the approriate exchange.

-----------------------

### Order processing ###

[Described here](https://github.com/Seriyin/Exchanger-Server)

Based on this specification front-end will:

- Forward trade orders to the relevant exchange.
- Forward subscription requests for up to 10 companies per user to the relevant exachanges.

-----------------------

### Authentication ###

 Authentication will be based on regular unique username + password pair convention.

 This means exchange front-end must:

- Allow client account creation.
- Alert client of username conflict on creation.
- Allow client login.
- Alert client of unexistant/incorrect credentials.
- Alert client on successful login.
- List available trading operations on successful login.

-----------------------

### Communication ###

Front-end server will be the client gateway to the [REST directory service](https://github.com/Seriyin/Exchanger-Directory).

Front-end must:

- Consult directory for traded company listing.
- Consult directory for known active exchanges.
- Consult directory for traded company's exchange.
- Consult directory for traded company daily statistics:
    - Opening price.
    - Closing price.
    - Minimum price traded for previous and current day.
    - Maximum price traded for previous and current day.


-----------------------

## Dependencies ##

Depends on [Chumak implementation of ZeroMQ](https://github.com/zeromq/chumak)

Dependencies are resolved through use of [rebar3](https://github.com/erlang/rebar3).
