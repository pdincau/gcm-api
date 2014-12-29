-module(subscription_notifier).
-export([process/1, process/3]).

-define(BASEURL, "http://yourcallback.com").
-define(ATTEMPTS_LIMIT, 5).
-define(RETRY_AFTER, 5000).

-include("utils.hrl").

process(Subscription) ->
    process(Subscription, ?RETRY_AFTER, 0).

process(_, _, ?ATTEMPTS_LIMIT) ->
    ok;

process(#subscription{appid=_AppId, userid=UserId, regid=_RegId} = Subscription, RetryAfter, Attempts) ->
    Json = jsx:encode(#{<<"userId">> => UserId}),
    case do_post(Json) of
        {error, Reason} ->
            error_logger:error_msg("Subscription not notified. Reason was: ~p~n", [Reason]),
            do_backoff(Subscription, RetryAfter, Attempts);
        _ ->
            error_logger:info_msg("Subscription successfully notified~n", []),
            ok
    end.

do_post(Request) ->
    try httpc:request(post, {?BASEURL, [], "application/json", Request}, [], []) of
        {ok, {{_, 200, _}, _Headers, _Body}} ->
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
    timer:apply_after(RetryAfter * 2, ?MODULE, process, [Subscription, RetryAfter * 2, Attempts + 1]),
    ok.
