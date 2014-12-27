-module(subscription_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = handle_request(Method, HasBody, Req2),
    {ok, Req3, State}.

handle_request(<<"POST">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    UserId = proplists:get_value(<<"userid">>, PostVals),
    RegId = proplists:get_value(<<"regid">>, PostVals),
    reply({UserId, RegId}, Req2);

handle_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body in request.">>, Req);

handle_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

reply({undefined, _}, Req) ->
    cowboy_req:reply(400, [], <<"Missing fields in body.">>, Req);

reply({_, undefined}, Req) ->
    cowboy_req:reply(400, [], <<"Missing fields in body.">>, Req);

reply({UserId, _RegId}, Req) ->
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], UserId, Req).

terminate(_Reason, _Req, _State) ->
    ok.
