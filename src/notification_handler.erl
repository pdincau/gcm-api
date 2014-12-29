-module(notification_handler).

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
    {ok, [{Payload, true}], Req2} = cowboy_req:body_qs(Req),
    error_logger:info_msg("Received notification: ~p~n", [Payload]),
    Update = jsx:decode(Payload),
    process(Update),
    error_logger:info_msg("Received update: ~p~n", [Update]),
    cowboy_req:reply(201, [], <<"">>, Req2);

handle_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"{\"error\":\"missing body\"}">>, Req);

handle_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.

process(Update) ->
    Fun = fun([{UserId, [{Key, Value}]}]) -> io:format("~p ~p ~p ~n", [UserId, Key, Value]) end,
    lists:map(Fun, Update).
