# Exchanger #

Erlang financial exchange front-end.

- [Exchanger](#exchanger)
    - [Purpose](#purpose)
    - [Overview](#overview)
        - [Order processing](#order-processing)
        - [Authentication](#authentication)
    - [Communication](#communication)
        - [Front-end <-> Directory](#front-end---directory)
        - [Front-end <-> Client](#front-end---client)
    - [Dependencies](#dependencies)

## Purpose ##

Is in charge of client authentication and redirecting buy and sell orders to the approriate exchange.

-----------------------

## Overview ##

### Order processing ###

[Described here](https://github.com/Seriyin/Exchanger-Server#order-processing)

Based on this specification front-end will:

- Forward trade orders to the relevant exchange.
- Forward ~~subscription requests for up to 10 companies per user to the relevant exchanges~~ relevant exchange address back to client **\***.

<sub> [*]  _For better performance exchanges publish directly to client unfortunatly making it impossible to globally restrict by company, only by exchange._ <sub>

-----------------------

### Authentication ###

[Described here](https://github.com/Seriyin/Exchanger-Client#authentication)

-----------------------

## Communication ##

_Front-end server will work with serialized pertinent information (client authentication requests, trade and consultation information) using [Protocol Buffers](https://github.com/google/protobuf)._


### Front-end <-> Directory ###

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

_**Note:** REST API requests and replies will use JSON format serialization for better Web compatibility._

-----------------------

### Front-end <-> Client ###



-----------------------

## Dependencies ##

- Depends on [Chumak native erlang implementation of ZeroMQ](https://github.com/zeromq/chumak).
- Depends on [Piqi](https://github.com/alavrik/piqi-erlang) for handling serialization natively on Erlang.

Dependencies are resolved through use of [rebar3](https://github.com/erlang/rebar3).

