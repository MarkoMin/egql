-module(graphql_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% Supervisor callbacks.
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @private
init([]) ->
    Flags = #{strategy=>one_for_all,
              intensity=>5,
              period=>3600},
    SchemaMgr = #{id=>graphql_schema,
                  start=>{graphql_schema, start_link, []}},
    {ok, {Flags, [SchemaMgr]}}.
