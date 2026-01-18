%% TODO use native records in OTP29+, provide `graphql_directive` module
%% to handle directives, rename `schema` field to `type` because it's
%% `directive_type()`.

%% Type Definition Language
-record(directive,
        { id :: graphql:name(),
          args = [] :: #{ binary() => term() } | [{any(), any()}],
          schema :: any() %% directive_type{}
        }).

-define(LAZY(X), {'$lazy', fun() -> X end}).

%% Only for not found behaviours
-define(RETURN_NULL, {ok, null}).
-define(RETURN_EMPTY_LIST, {ok, []}).
