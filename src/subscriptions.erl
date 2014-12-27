-module(subscriptions).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add/1]).

-define(SERVER, ?MODULE).

-record(state, {subscriptions}).

-include("utils.hrl").

add(#subscription{appid=_AppId, userid=_UserId, regid=_RegId} = Subscription) ->
    error_logger:info_msg("Adding subscription~n", []),
    gen_server:call(?SERVER, {add, Subscription}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{subscriptions=[]}}.

handle_call({add, Subscription}, _From, #state{subscriptions=Subscriptions} = State) ->
    NewState = State#state{subscriptions=[Subscription|Subscriptions]},
    {reply, {ok, subscribed}, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
