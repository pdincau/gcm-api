-module(subscription_notifier).
-export([process/2, process/4]).

-define(ATTEMPTS_LIMIT, 5).
-define(RETRY_AFTER, 5000).

-include("utils.hrl").

process(AppName, Subscription) ->
    process(AppName, Subscription, ?RETRY_AFTER, 0).

process(_, _, _, ?ATTEMPTS_LIMIT) ->
    ok;

process(AppName, Subscription, RetryAfter, Attempts) ->
    Application = applications:get(AppName),
    #subscription{userid=UserId, regid=_RegId} = Subscription,
    Json = jsx:encode(#{<<"username">> => UserId}),
    case do_post(Application, Json) of
        {error, Reason} ->
            error_logger:error_msg("Subscription not notified. Reason was: ~p~n", [Reason]),
            do_backoff(Subscription, RetryAfter, Attempts),
            ok;
        _ ->
            error_logger:info_msg("Subscription successfully notified~n", [])
    end.

do_post(Application, Request) ->
    Url = maps:get(<<"callback_url">>, Application),
    try httpc:request(post, {Url, [], "application/json", Request}, [], []) of
        {ok, {{_, 201, _}, _Headers, _Body}} ->
            ok;
        {ok, {{_, _, _}, _, _}} ->
            {error, bad_status};
        {error, Reason} ->
            {error, Reason};
        Error ->
            {error, Error}
    catch
        Exception ->
	    {error, Exception}
    end.

do_backoff(Subscription, RetryAfter, Attempts) ->
    NewRetryAfter = RetryAfter * 2,
    timer:apply_after(NewRetryAfter, ?MODULE, process, [Subscription, NewRetryAfter, Attempts + 1]).
