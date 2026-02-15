-module(graphql_scalar_integer_coerce).

-export([input/2, output/2]).

-define(MAX_INT, (1 bsl 31)).
-define(MIN_INT, (-(1 bsl 31) - 1)).

input(_, X) when is_integer(X) ->
    check_int32(X);
input(_, X) when is_float(X) ->
    float_to_int32(X);
input(_, X) ->
    {ok, X}.

output(<<"Int">>, I) when is_integer(I) ->
    check_int32(I);
output(<<"Int">>, F) when is_float(F) ->
    float_to_int32(F);
output(_, _) ->
    {ok, null}.

%% assumes X is integer
check_int32(X) when X > ?MAX_INT -> {error, not_int32_value};
check_int32(X) when X < ?MIN_INT -> {error, not_int32_value};
check_int32(X) -> {ok, X}.

float_to_int32(X) ->
    maybe
        {ok, Int} ?= float_to_int(X),
        check_int32(Int)
    end.

float_to_int(X) ->
    T = trunc(X),
    case T == X of
        true -> {ok, T};
        _ -> {error, float_truncate_not_integer}
    end.
