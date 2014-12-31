-module(subscriptions).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add/2, find/2]).

-record(state, {tid}).

-include("utils.hrl").

add(AppName, #subscription{userid=_UserId, regid=_RegId} = Subscription) ->
    error_logger:info_msg("Adding subscription: ~p~n", [Subscription]),
    Pid = pid_for(AppName),
    gen_server:call(Pid, {add, Subscription}).

find(AppName, UserId) ->
    error_logger:info_msg("Recovering subscription for userid: ~p~n", [UserId]),
    Pid = pid_for(AppName),
    gen_server:call(Pid, {find, UserId}).

start_link(AppName) ->
    gen_server:start_link(?MODULE, [AppName], []).

init([AppName]) ->
    gen_server:cast(self(), {setup, AppName}),
    {ok, #state{}}.

handle_call({add, Subscription}, _From, #state{tid=Tid} = State) ->
    ets:insert(Tid, Subscription),
    {reply, {ok, subscribed}, State};

handle_call({find, UserId}, _From, #state{tid=Tid} = State) ->
    case ets:lookup(Tid, UserId) of
        [Subscription] ->
            {reply, {ok, Subscription}, State};
        [] ->
            {reply, {error, no_subscription}, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({setup, AppName}, _State) ->
    gproc:reg({n, l, {?MODULE, AppName}}),
    Tid = ets:new(?MODULE, [set, {keypos, #subscription.userid}]),
    {noreply, #state{tid=Tid}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

pid_for(AppName) ->
    gproc:lookup_pid({n, l, {?MODULE, AppName}}).
