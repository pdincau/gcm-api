-module(application_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(FIELDS, [<<"name">>, <<"callback_url">>]).

init(_Transport, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req2),
    {ok, Req3} = handle_request(Method, HasBody, Req2),
    {ok, Req3, State}.

handle_request(<<"POST">>, true, Req) ->
    {ok, [{Payload, true}], Req2} = cowboy_req:body_qs(Req),
    error_logger:info_msg("Received request with body ~p~n", [Payload]),
    Application = application_from(Payload),
    case errors_in(Application) of
        [] ->
            %%TODO: do something cool with Application ;)
            cowboy_req:reply(201, [], <<"">>, Req2);
        Errors ->
            cowboy_req:reply(400, [], body_for(Errors), Req2)
    end;

handle_request(<<"POST">>, false, Req) ->
    cowboy_req:reply(400, [], <<"{\"error\":\"missing body\"}">>, Req);

handle_request(_, _, Req) ->
    cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) ->
    ok.

errors_in(Application) ->
    errors_in(?FIELDS, Application, []).

errors_in([], _, Errors) ->
    Errors;

errors_in([Key|Keys], Application, Errors) ->
    case maps:find(Key, Application) of
        error ->
            errors_in(Keys, Application, [{Key, <<"missing">>}|Errors]);
        {ok, <<>>} ->
            errors_in(Keys, Application, [{Key, <<"can't be blank">>}|Errors]);
        _ ->
            errors_in(Keys, Application, Errors)
    end.

application_from(Payload) ->
    jsx:decode(Payload, [return_maps]).

body_for(Errors) ->
    Pre = <<"{\"errors\":[">>, JsonErrors = jsx:encode(Errors), Post = <<"]}">>,
    <<Pre/binary, JsonErrors/binary, Post/binary>>.
