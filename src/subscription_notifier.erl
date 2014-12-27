-module(subscription_notifier).
-export([process/1]).

-record(subscription, {appid, userid, regid}).

process(#subscription{appid=_AppId, userid=_UserId, regid=_RegId} = _Subscription) ->
    error_logger:info_msg("Notify external application of subscription~n", []),
    ok.
