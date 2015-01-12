-module(applications).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add/1]).

-define(SERVER, ?MODULE).

-record(state, {connection}).

add(Application) ->
    error_logger:info_msg("Adding applicaton: ~p~n", [Application]),
    gen_server:call(?SERVER, {add, Application}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    gen_server:cast(self(), setup),
    {ok, #state{}}.

handle_call({add, Application}, _From, #state{connection=Connection} = State) ->
    %% following line returns: {ok, return_value()} | {error, Reason::binary() | no_connection}.
    eredis:q(Connection, ["SET", <<"a_key">>, jsx:encode(Application)]),
    Reply = ok,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(setup, State) ->
    {ok, Connection} = eredis:start_link(),
    NewState = State#state{connection=Connection},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
