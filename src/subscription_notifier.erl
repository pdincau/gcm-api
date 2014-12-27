-module(subscription_notifier).
-export([process/1]).

-include("utils.hrl").

process(#subscription{appid=_AppId, userid=_UserId, regid=_RegId} = _Subscription) ->
    error_logger:info_msg("Notify external application of subscription~n", []),
    ok.
