-module(graphql_introspection).

-include_lib("stdlib/include/qlc.hrl").
-include("graphql_schema.hrl").
-include_lib("graphql/include/graphql.hrl").

-export([inject/0, augment_root/1]).

-export([execute/4]).

%% Resolvers for the introspection
-export([
    schema_resolver/4,
    type_resolver/4,
    schema_types/4,
    query_type/4,
    mutation_type/4,
    subscription_type/4
]).

-spec augment_root(QueryObj) -> ok
  when QueryObj :: binary().

augment_root(QName) ->
    #object_type{ fields = Fields } = Obj = graphql_schema:get(QName),
    Schema = #schema_field {
                ty = {non_null, <<"__Schema">>},
                description = <<"The introspection schema">>,
                resolve = fun ?MODULE:schema_resolver/4
               },
    Type = #schema_field {
              ty = <<"__Type">>,
              description = <<"The introspection type">>,
              args = #{
                <<"name">> =>
                    #schema_arg { ty = {non_null, <<"String">>},
                                  description = <<"The type to query">> }
               },
              resolve = fun ?MODULE:type_resolver/4 },
    Augmented = Obj#object_type { fields = Fields#{
                                             <<"__schema">> => Schema,
                                             <<"__type">> => Type
                                            }},
    ok = graphql_schema:insert(Augmented, #{}),
    ok.

schema_resolver(_Ctx, none, _, #{}) ->
    {ok, #{<<"directives">> => render_directives()}}.

type_resolver(_Ctx, none, _, #{ <<"name">> := N }) ->
    case graphql_schema:lookup(N) of
        not_found -> {ok, null};
        Ty -> render_type(Ty)
    end.

query_type(_Ctx, _Obj, _, _) ->
    #root_schema{ query = QType } = graphql_schema:get('ROOT'),
    render_type(QType).

mutation_type(_Ctx, _Obj, _, _) ->
    #root_schema { mutation = MType } = graphql_schema:get('ROOT'),
    case MType of
        undefined -> {ok, null};
        MT -> render_type(MT)
    end.

subscription_type(_Ctx, _Obj, _, _) ->
    #root_schema { subscription = SType } = graphql_schema:get('ROOT'),
    case SType of
        undefined -> {ok, null};
        ST -> render_type(ST)
    end.

schema_types(_Ctx, _Obj, _, _Args) ->
    Pass = fun
        (#root_schema{}) -> false;
        (#directive_type{}) -> false;
        (_) -> true
    end,
    Types = [X || X <- graphql_schema:all(), Pass(X)],
    {ok, [render_type(Ty) || Ty <- Types]}.

%% Main renderer. Calls out to the subsets needed
render_type(Name) when is_binary(Name) ->
    case graphql_schema:lookup(Name) of
        not_found ->
           throw({not_found, Name});
       Ty -> render_type(Ty)
    end;
render_type(Ty) -> {ok, #{
    <<"kind">> => type_kind(Ty),
    <<"name">> => type_name(Ty),
    <<"description">> => type_description(Ty),
    <<"fields">> => type_fields(Ty),
    <<"interfaces">> => type_interfaces(Ty),
    <<"possibleTypes">> => type_possibilities(Ty),
    <<"enumValues">> => type_enum_values(Ty),
    <<"inputFields">> => type_input_fields(Ty),
    <<"ofType">> => type_unwrap(Ty) }}.

type_kind(#scalar_type{}) -> <<"SCALAR">>;
type_kind(#object_type {}) -> <<"OBJECT">>;
type_kind(#input_object_type {}) -> <<"INPUT_OBJECT">>;
type_kind(#interface_type{}) -> <<"INTERFACE">>;
type_kind(#union_type{}) -> <<"UNION">>;
type_kind(#enum_type{}) -> <<"ENUM">>;
type_kind({list, _}) -> <<"LIST">>;
type_kind({non_null, _Ty}) -> <<"NON_NULL">>.

type_name(#scalar_type { id = N }) -> N;
type_name(#enum_type { id = N }) -> N;
type_name(#interface_type { id = N }) -> N;
type_name(#object_type { id = N }) -> N;
type_name(#input_object_type { id = N }) -> N;
type_name(#union_type { id = N }) -> N;
type_name({non_null, _}) -> null;
type_name({list, _}) -> null.

type_description(#object_type { description = D }) -> D;
type_description(#input_object_type { description = D }) -> D;
type_description(#interface_type { description = D}) -> D;
type_description(#enum_type{ description = D}) -> D;
type_description(#scalar_type{ description = D}) -> D;
type_description(_) -> null.

type_interfaces(#object_type{ interfaces = IFs }) ->
    ?LAZY({ok, [render_type(Ty) || Ty <- IFs]});
type_interfaces(_) -> null.

type_possibilities(#interface_type { id = ID }) ->
    ?LAZY(interface_implementors(ID));
type_possibilities(#union_type { types = Types }) ->
    ?LAZY({ok, [render_type(Ty) || Ty <- Types]});
type_possibilities(_) -> null.

type_enum_values(#enum_type { values = VMap }) ->
    Values0 = [render_enum_value(V) || V <- maps:to_list(VMap)],
    unwrap_list_render(Values0);
type_enum_values(_) -> null.

unwrap_list_render(Values) ->
    [unwrap_render(Val) || Val <- Values].

unwrap_render({ok, Val}) -> Val.

type_unwrap({list, Ty}) -> unwrap_render(render_type(Ty));
type_unwrap({non_null, Ty}) -> unwrap_render(render_type(Ty));
type_unwrap(_) -> null.

type_input_fields(#input_object_type{ fields = FS }) ->
    ?LAZY({ok, [render_input_value(F) || F <- maps:to_list(FS)]});
type_input_fields(_) -> null.

type_fields(#object_type { fields = FS }) ->
    ?LAZY({ok, [render_field(F) || F <- maps:to_list(FS), interesting_field(F)]});
type_fields(#interface_type { fields = FS }) ->
    ?LAZY({ok, [render_field(F) || F <- maps:to_list(FS), interesting_field(F)]});
type_fields(_) -> null.

interesting_field({<<"__schema">>, #schema_field {}}) -> false;
interesting_field({<<"__type">>, #schema_field{}}) -> false;
interesting_field({_, _}) -> true.

render_field({Name, #schema_field {
                       description = Desc,
                       args = Args,
                       ty = Ty,
                       directives = Directives}
             }) ->
    {IsDeprecated, DeprecationReason} = render_deprecation(Directives),
    {ok, #{
        <<"name">> => Name,
        <<"description">> => Desc,
        <<"args">> => ?LAZY({ok, [render_input_value(IV) || IV <- maps:to_list(Args)]}),
        <<"type">> => ?LAZY(render_type(Ty)),
        <<"isDeprecated">> => IsDeprecated,
        <<"deprecationReason">> => DeprecationReason
      }}.

render_input_value({K, #schema_arg { ty = Ty,
                                     description = Desc,
                                     default = Default }}) ->
    {ok, #{
        <<"name">> => K,
        <<"description">> => Desc,
        <<"type">> => ?LAZY(render_type(Ty)),
        <<"defaultValue">> => render_optional(Default)
    }}.

render_optional(undefined) -> null;
render_optional(Value) -> Value.

interface_implementors(ID) ->
    Pass = fun
        (#object_type { interfaces = IFs }) -> lists:member(ID, IFs);
        (_) -> false
    end,
    {ok, [render_type(Ty) || Ty <- graphql_schema:all(), Pass(Ty)]}.

render_enum_value({_Value, #enum_value{
                              val = Key,
                              description = Desc,
                              directives = Directives}
                  }) ->
    {IsDeprecated, DeprecationReason} = render_deprecation(Directives),
    {ok, #{
       <<"name">> => Key,
       <<"description">> => Desc,
       <<"isDeprecated">> => IsDeprecated,
       <<"deprecationReason">> => DeprecationReason
     }}.

render_deprecation([]) ->
    {false, null};
render_deprecation([#directive{id = <<"deprecated">>, args = Args, schema = Type} | _]) ->
    Reason = case Args of
                 #{<<"reason">>:=R} -> R;
                 _ ->
                     ReasonArg = maps:get(<<"reason">>, Type#directive_type.args),
                     ReasonArg#schema_arg.default
             end,
    {true, Reason};
render_deprecation([_ | Rest]) ->
    render_deprecation(Rest).

%% -- SCHEMA DEFINITION -------------------------------------------------------
-spec inject() -> ok.
inject() ->
    Schema = {object, #{
                id => '__Schema',
                resolve_module => ?MODULE,
                description => "Schema Definitions in GraphQL",
                fields => #{
                  types => #{
                    type => {non_null, ['__Type!']},
                    description => "The types in the Schema that are defined",
                    resolve => fun ?MODULE:schema_types/4 },
                  queryType => #{
                    type => '__Type!',
                    description => "The type of the 'query' entries.",
                    resolve => fun ?MODULE:query_type/4 },
                  mutationType => #{
                    type => '__Type',
                    description => "The type of the 'mutation' entries.",
                    resolve => fun ?MODULE:mutation_type/4 },
                  subscriptionType => #{
                    type => '__Type',
                    description => "The type of the 'subscription' entries.",
                    resolve => fun ?MODULE:subscription_type/4 },
                  directives => #{
                    type => {non_null, ['__Directive!']},
                    description => "The directives the server currently understands"
                   }}}},
    Type = {object, #{
              id => '__Type',
              resolve_module => ?MODULE,
              description => "Descriptions of general types/objects in the model",
              fields => #{
                kind => #{
                  type => '__TypeKind!',
                  description => "The general kind of the type" },
                name => #{
                  type => 'String',
                  description => "The name of the type" },
                description => #{
                  type => 'String',
                  description => "The description field of the type" },
                %% OBJECT and INTERFACE only
                fields => #{
                  type => ['__Field!'],
                  description => "The fields in the object/interface",
                  args => #{
                    includeDeprecated => #{
                      type => 'Bool',
                      description => "Should deprecated fields be included or not",
                      default => false
                     }}},
                %% OBJECT only
                interfaces => #{
                  type => ['__Type!'],
                  description => "The interfaces the object adheres to"
                 },
                %% INTERFACE and UNION only
                possibleTypes => #{
                  type => ['__Type!'],
                  description => "The possible types of an interface and/or union"
                 },
                %% ENUM
                enumValues => #{
                  type => ['__EnumValue!'],
                  description => "The possible values of an Enum",
                  args => #{
                    includeDeprecated => #{
                      type => 'Bool',
                      description => "Should deprecated fields be included or not",
                      default => false
                     }}},
                %% INPUT OBJECT ONLY
                inputFields => #{
                  type => ['__InputValue!'],
                  description => "The possible inputfields of the Input Object" },
                %% NON NULL / LIST
                ofType => #{
                  type => '__Type',
                  description => "The underlying type of a non-null or list type" }
               }}},
    Field = {object, #{
               id => '__Field',
               resolve_module => ?MODULE,
               description => "Fields in a Schema",
               fields => #{
                 name => #{
                   type => 'String!',
                   description => "The name of the field" },
                 description => #{
                   type => 'String',
                   description => "The description of a field" },
                 args => #{
                   type => ['__InputValue!'],
                   description => "The possible arguments of a given field" },
                 type => #{
                   type => '__Type',
                   description => "The type of the given field" },
                 isDeprecated => #{
                   type => 'Bool!',
                   description => "True if the field is deprecated, false otherwise" },
                 deprecationReason => #{
                   type => 'String',
                   description => "The Reason for the deprecation, if any" }
                }}},
    InputValue = {object, #{
                    id => '__InputValue',
                    resolve_module => ?MODULE,
                    description => "InputValues in the Schema",
                    fields => #{
                      name => #{
                        type => 'String!',
                        description => "The name of the inputvalue" },
                      description => #{
                        type => 'String',
                        description => "The description of the field" },
                      type => #{
                        type => '__Type!',
                        description => "The type of the field" },
                      defaultValue => #{
                        type => 'String',
                        description => "The default value of the field rendered as a string" }
                     }}},
    Enum = {object, #{
              id => "__EnumValue",
              resolve_module => ?MODULE,
              description => "Introspection of enumeration values",
              fields => #{
                name => #{
                  type => 'String!',
                  description => "The name of the enumeration value" },
                description => #{
                  type => 'String',
                  description => "The description of the value" },
                isDeprecated => #{
                  type => 'Bool!',
                  description => "True if this is a deprecated field" },
                deprecationReason => #{
                  type => 'String',
                  description => "The reason for the deprecation, if any" }
               }}},
    TypeKind = {enum, #{
                  id => '__TypeKind',
                  description => "The different type schemes (kinds)",
                  resolve_module => graphql_enum_coerce,
                  values => #{
                    'SCALAR' => #{ value => 1, description => "Scalar types" },
                    'OBJECT' => #{ value => 2, description => "Object types" },
                    'INTERFACE' => #{ value => 3, description => "Interface types" },
                    'UNION' => #{ value => 4, description => "Union types" },
                    'ENUM' => #{ value => 5, description => "Enum types" },
                    'INPUT_OBJECT' => #{ value => 6, description => "InputObject types" },
                    'LIST' => #{ value => 7, description => "List types" },
                    'NON_NULL' => #{ value => 8, description => "NonNull types" }
                   }}},
    Directive = {object, #{
                   id => '__Directive',
                   resolve_module => ?MODULE,
                   description => "Representation of directives",
                   fields => #{
                     name => #{
                       type => 'String!',
                       description => "The name of the directive" },
                     description => #{
                       type => 'String',
                       description => "The description of the directive" },
                     locations => #{
                       type => {non_null, ['__DirectiveLocation!']},
                       description => "Where the directives can be used" },
                     onOperation => #{
                        type => 'Bool',
                        description => "Not documented yet" },
                     onFragment => #{
                        type => 'Bool',
                        description => "Not documented yet" },
                     onField => #{
                        type => 'Bool',
                        description => "Not documented yet" },
                     args => #{
                       type => {non_null, ['__InputValue!']},
                       description => "The possible arguments for the given directive" }
                    }}},
    Locations = #{Loc => #{value=>Idx, description => location_desc(Loc)}
                  || {Idx, Loc} <- lists:enumerate(?DIRECTIVE_LOCATIONS)},
    DirectiveLocation = {enum, #{
                           id => '__DirectiveLocation',
                           description => "Where a given directive can be used",
                           resolve_module => graphql_enum_coerce,
                           values => Locations}},
    ok = graphql:insert_schema_definition(DirectiveLocation),
    ok = graphql:insert_schema_definition(Directive),
    ok = graphql:insert_schema_definition(TypeKind),
    ok = graphql:insert_schema_definition(Enum),
    ok = graphql:insert_schema_definition(InputValue),
    ok = graphql:insert_schema_definition(Field),
    ok = graphql:insert_schema_definition(Type),
    ok = graphql:insert_schema_definition(Schema),
    ok.

location_desc('QUERY') -> "Queries";
location_desc('MUTATION') -> "Mutations";
location_desc('FIELD') -> "Fields";
location_desc('FRAGMENT_DEFINITION') -> "Fragment definitions";
location_desc('FRAGMENT_SPREAD') -> "Fragment spread";
location_desc('INLINE_FRAGMENT') -> "Inline fragments";
location_desc('FIELD_DEFINITION') -> "Field definitions";
location_desc('ENUM_VALUE') -> "Enum values";
location_desc('SUBSCRIPTION') -> "Subscriptions";
location_desc('SCHEMA') -> "Schema";
location_desc('SCALAR') -> "Scalars";
location_desc('OBJECT') -> "Objects";
location_desc('ARGUMENT_DEFINITION') -> "Argument definitions";
location_desc('INTERFACE') -> "Argument definitions";
location_desc('UNION') -> "Unions";
location_desc('ENUM') -> "Enums";
location_desc('INPUT_OBJECT') -> "Input objects";
location_desc('INPUT_FIELD_DEFINITION') -> "Input field definitions";
location_desc('VARIABLE_DEFINITION') -> "Variable definitions".

render_directives() ->
    %% TODO this might be slow
    Objects = graphql_schema:all(),
    Directives = [Id || #directive_type{id = Id} <- Objects],
    Rendered0 = [render_directive(graphql_schema:get(Directive))
                 || Directive <- Directives],
    unwrap_list_render(Rendered0).

render_directive(#directive_type{id = Name,
                          description = Desc,
                          locations = Locations,
                          args = Args0}) ->
    RenderedArgs0 = [render_input_value({ArgName, Arg}) || ArgName := Arg <- Args0],
    RenderedArgs = unwrap_list_render(RenderedArgs0),
    {ok, #{
       <<"name">> => Name,
       <<"description">> => render_optional(Desc),
       <<"locations">> => [atom_to_binary(Loc) || Loc <- Locations],
       <<"args">> => RenderedArgs
     }}.

%% Resolver for introspection
execute(_Ctx, null, _, _) ->
    {ok, null};
execute(_Ctx, Obj, FieldName, _Args) ->
    case maps:get(FieldName, Obj, not_found) of
        {'$lazy', F} when is_function(F, 0) -> F();
        not_found -> {error, not_found};
        Values when is_list(Values) -> {ok, [ {ok, R} || R <- Values ]};
        Value -> {ok, Value}
    end.
