-module(subscription_notifier).
-export([process/1]).

-define(BASEURL, "http://yourcallback.com").

-include("utils.hrl").

process(#subscription{appid=_AppId, userid=UserId, regid=_RegId} = _Subscription) ->
    error_logger:info_msg("Notify external application of subscription~n", []),
    Json = jsx:encode(#{<<"userId">> => UserId}),
    try httpc:request(post, {?BASEURL, [], "application/json", Json}, [], []) of
        {ok, {{_, 200, _}, _Headers, _Body}} ->
            error_logger:info_msg("Subscription successfully notified~n", []);
        {ok, {{_, _, _}, _, _}} ->
	    error_logger:error_msg("Error in request.~n", []);
        {error, Reason} ->
	    error_logger:error_msg("Error in request. Reason was: ~p~n", [Reason]);
        OtherError ->
	    error_logger:error_msg("Error in request. Reason was: ~p~n", [OtherError])
    catch
        Exception ->
	    error_logger:error_msg("Error in request. Exception ~p~n", [Exception])
    end.
