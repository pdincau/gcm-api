-module(notification_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("utils.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = handle_request(Method, HasBody, Req2),
    {ok, Req3, State}.

handle_request(<<"POST">>, true, Req) ->
    {ok, [{Payload, true}], Req2} = cowboy_req:body_qs(Req),
    {AppName, Req3} = cowboy_req:binding(app_name, Req2),
    process(binary_to_existing_atom(AppName, utf8), jsx:decode(Payload)),
    cowboy_req:reply(201, [], <<"">>, Req3);

handle_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"{\"error\":\"missing body\"}">>, Req);

handle_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.

process(AppName, Update) ->
    error_logger:info_msg("Processing notifications: ~p~n", [Update]),
    lists:map(fun(Notification) -> send(AppName, Notification) end, Update).

send(AppName, [{UserId, [{Key, Value}]}]) ->
    case subscriptions:find(UserId) of
        {error, _} ->
            error_logger:error_msg("UserId: ~p not found.~n", [UserId]);
        {ok, Subscription} ->
            Message = [{<<"data">>, [{Key, Value}]}],
            gcm:push(AppName, [Subscription#subscription.regid], Message)
    end.
