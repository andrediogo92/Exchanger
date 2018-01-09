-define(PORT_EXCHANGE,10000).
-define(PORT_CLIENTS,10001).
-define(DIRECTORY,{<<"localhost">>,9999}).
-define(NUM_RETRIES,5).
-define(TIME_OUT,120000).
-define(STUPID_TIME_OUT,1200000).
-define(EXCHANGER_NAME,exchanger).
-define(CLIENTER_NAME, clienter).
-define(LOGINER_NAME,loginer).
-define(KNOWN, #{<<"Adolph">> => {<<"1111">>, false}, 
                 <<"Bert">> => {<<"2222">>, false},
                 <<"Carlos">> => {<<"3333">>, false},
                 <<"Diego">> => {<<"4444">>, false},
                 <<"Earl">> => {<<"5555">>, false},
                 <<"Frederick">> => {<<"6666">>, false},
                 <<"Gonzo">> => {<<"7777">>, false}}).
