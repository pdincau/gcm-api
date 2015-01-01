-module(subscriptions_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type, AppName), {I, {I, start_link, [AppName]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Procs = [?CHILD(subscriptions, worker, <<"appname">>)],
    {ok, {{one_for_one, 0, 1}, Procs}}.
