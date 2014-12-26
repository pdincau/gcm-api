-module(gcm_api_app).

-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    gcm_api_sup:start_link().

stop(_State) ->
    ok.
