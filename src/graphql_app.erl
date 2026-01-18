-module(graphql_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> supervisor:startlink_ret().
start(_Type, _Args) ->
    graphql_sup:start_link().

-spec stop([]) -> ok.
stop(_State) ->
    ok.
