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

process(Subscription, RetryAfter, Attempts) ->
    error_logger:info_msg("Notify external application of subscription~n", []),
    #subscription{appid=_AppId, userid=UserId, regid=_RegId} = Subscription,
    Json = jsx:encode(#{<<"userId">> => UserId}),
    try httpc:request(post, {?BASEURL, [], "application/json", Json}, [], []) of
        {ok, {{_, 200, _}, _Headers, _Body}} ->
            error_logger:info_msg("Subscription successfully notified~n", []);
        {ok, {{_, _, _}, _, _}} ->
	    error_logger:error_msg("Error in request.~n", []),
            do_backoff(Subscription, RetryAfter, Attempts);
        {error, Reason} ->
	    error_logger:error_msg("Error in request. Reason was: ~p~n", [Reason]),
            do_backoff(Subscription, RetryAfter, Attempts);
        OtherError ->
	    error_logger:error_msg("Error in request. Reason was: ~p~n", [OtherError]),
            do_backoff(Subscription, RetryAfter, Attempts)
    catch
        Exception ->
	    error_logger:error_msg("Error in request. Exception ~p~n", [Exception]),
            do_backoff(Subscription, RetryAfter, Attempts)
    end.

do_backoff(Subscription, RetryAfter, Attempts) ->
    timer:apply_after(RetryAfter * 2, ?MODULE, process, [Subscription, RetryAfter * 2, Attempts + 1]),
    ok.
