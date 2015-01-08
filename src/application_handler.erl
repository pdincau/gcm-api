-module(application_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(application, {name}).

-define(FIELDS, [<<"name">>]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = handle_request(Method, HasBody, Req2),
    {ok, Req3, State}.

handle_request(<<"POST">>, true, Req) ->
    {ok, Body, Req2} = cowboy_req:body_qs(Req),
    _Application = application_from(Body),
    cowboy_req:reply(201, [], <<"">>, Req2);

handle_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"{\"error\":\"missing body\"}">>, Req);

handle_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.

application_from(_Body) ->
    ok.
    %%UserId = proplists:get_value(<<"userid">>, PostVals),
    %%#application{appname=AppName}.
