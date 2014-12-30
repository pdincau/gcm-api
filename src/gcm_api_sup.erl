-module(gcm_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(I, Type), {I, {I, start_link, [<<"appname">>]}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Procs = [?CHILD(subscriptions, worker)],
    {ok, {{one_for_one, 0, 1}, Procs}}.

