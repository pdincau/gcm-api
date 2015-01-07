-module(gcm_api_app).

-behaviour(application).

-export([start/2
        ,stop/1]).

start(_StartType, _StartArgs) ->
    start_cowboy(),
    start_gcm_client(),
    gcm_api_sup:start_link().

stop(_State) ->
    ok.

start_cowboy() ->
    TransOpts = [{port, 8080}],
    Routes = routes(),
    Dispatch = cowboy_router:compile(Routes),
    ProtoOpts = [{env, [{dispatch, Dispatch}]}],
    {ok, _} = cowboy:start_http(http, 100, TransOpts, ProtoOpts).

routes() ->
    [{'_', [{"/:app_name/subscriptions", subscription_handler, []}
           ,{"/:app_name/notifications", notification_handler, []}
           ,{"/applications/", application_handler, []}]}].

start_gcm_client() ->
    gcm:start(appname, "APIKEY").
