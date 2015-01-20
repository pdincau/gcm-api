-module(subscriptions_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(AppName) ->
        supervisor:start_child(?MODULE, [AppName]).

init([]) ->
    spawn_link(fun start_children/0),
    Procs = [?CHILD(subscriptions, worker)],
    {ok, {{simple_one_for_one, 0, 1}, Procs}}.

start_children() ->
    {ok, Applications} = applications:keys(),
    [start_child(AppName) || AppName <- Applications],
    ok.
