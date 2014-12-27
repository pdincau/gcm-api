-module(subscriptions).
-export([add/1]).

-include("utils.hrl").

add(#subscription{appid=_AppId, userid=_UserId, regid=_RegId} = _Subscription) ->
    error_logger:info_msg("Adding subscription~n", []),
    {ok, subscribed}.
