-module(subscription_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(FIELDS, [<<"userid">>, <<"regid">>]).

-include("utils.hrl").

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = handle_request(Method, HasBody, Req2),
    {ok, Req3, State}.

handle_request(<<"POST">>, true, Req) ->
    {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
    case validate_presence(?FIELDS, PostVals) of
        [] ->
            reply(PostVals, Req2);
        _Errors ->
            %%TODO: improve error message with error messages
            cowboy_req:reply(400, [], <<"Missing fields in body.">>, Req2)
    end;

handle_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"Missing body in request.">>, Req);

handle_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

reply(PostVals, Req) ->
    Subscription = subscription_from(PostVals),
    {AppName, Req2} = cowboy_req:binding(app_name, Req),
    {ok, subscribed} = subscriptions:add(AppName, Subscription),
    spawn(fun() -> subscription_notifier:process(Subscription) end),
    cowboy_req:reply(201, [], <<"">>, Req2).

terminate(_Reason, _Req, _State) ->
    ok.

validate_presence(Keys, Fields) ->
    validate_presence(Keys, Fields,  []).

validate_presence([], _, Errors) ->
    Errors;

validate_presence([Key|Keys], Fields, Errors) ->
    case proplists:get_value(Key, Fields) of
        undefined ->
            validate_presence(Keys, Fields, [{Key, <<"can't be blank">>}|Errors]);
        _ ->
            validate_presence(Keys, Fields, Errors)
    end.

subscription_from(PostVals) ->
    UserId = proplists:get_value(<<"userid">>, PostVals),
    RegId = proplists:get_value(<<"regid">>, PostVals),
    #subscription{userid=UserId, regid=RegId}.
