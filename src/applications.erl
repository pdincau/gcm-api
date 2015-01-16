-module(applications).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add/1, get/1, delete/1]).

-define(SERVER, ?MODULE).

-record(state, {connection}).

add(Application) ->
    gen_server:call(?SERVER, {add, Application}).

get(AppName) ->
    gen_server:call(?SERVER, {get, AppName}).

delete(AppName) ->
    gen_server:call(?SERVER, {delete, AppName}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    self() ! setup,
    {ok, #state{}}.

handle_call({add, Application}, _From, #state{connection=Connection} = State) ->
    AppName = maps:get(<<"name">>, Application),
    Reply = eredis:q(Connection, ["SET", AppName, jsx:encode(Application)]),
    error_logger:info_msg("Add application ~p. Result ~p~n", [Application, Reply]),
    {reply, Reply, State};

handle_call({get, AppName}, _From, #state{connection=Connection} = State) ->
    Reply = case eredis:q(Connection, ["GET", AppName]) of
        {error, no_connection} ->
            {error, no_connection};
        {ok, Json} ->
            {ok, jsx:decode(Json, [return_maps])}
    end,
    error_logger:info_msg("Get appname ~p. Result ~p~n", [AppName, Reply]),
    {reply, Reply, State};

handle_call({delete, AppName}, _From, #state{connection=Connection} = State) ->
    Reply = case eredis:q(Connection, ["DEL", AppName]) of
        {error, no_connection} ->
            {error, no_connection};
        {ok, Result} ->
            {ok, Result}
    end,
    error_logger:info_msg("Remove application ~p. Result ~p~n", [Application, Reply]),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(setup, State) ->
    {ok, Connection} = eredis:start_link(),
    NewState = State#state{connection=Connection},
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
