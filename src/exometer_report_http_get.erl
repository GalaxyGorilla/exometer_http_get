-module(exometer_report_http_get).

-behaviour(exometer_report).

%% gen_server callbacks
-export([exometer_init/1,
         exometer_info/2,
         exometer_cast/2,
         exometer_call/3,
         exometer_report/5,
         exometer_subscribe/5,
         exometer_unsubscribe/4,
         exometer_newentry/2,
         exometer_setopts/4,
         exometer_terminate/2]).


-include_lib("exometer_core/include/exometer.hrl").

-define(DEFAULT_HOST, {127, 0, 0, 1}).
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_AUTOSUBSCRIBE, false).
-define(DEFAULT_SUBSCRIPTIONS_MOD, undefined).

-record(state, {host :: inet:ip_address() | inet:hostname(),
                port :: inet:port_number(),  % for udp
                autosubscribe :: boolean(),
                subscriptions_module :: module(),
                subscriptions ::  map()}).


%% ===================================================================
%% Public API
%% ===================================================================
exometer_init(Opts) ->
    Host = proplists:get_value(host, Opts, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Opts, ?DEFAULT_PORT),
    Autosubscribe = proplists:get_value(autosubscribe, Opts, ?DEFAULT_AUTOSUBSCRIBE),
    SubscriptionsMod = proplists:get_value(subscriptions_module, Opts, ?DEFAULT_SUBSCRIPTIONS_MOD),
    State = #state{host = Host,
                   port = Port,
                   autosubscribe = Autosubscribe,
                   subscriptions_module = SubscriptionsMod,
                   subscriptions = maps:new()},
    Dispatch = cowboy_router:compile([{'_', [{"/[...]", cowboy_http_get_handler, []}]}]),
    Result = cowboy:start_http(cowboy_http_get_handler, 100, [{ip, Host}, {port, 8080}],
                               [{env, [{dispatch, Dispatch}]}]),
    case Result of
        {ok, _} -> {ok, State};
        Error   -> Error
    end.

exometer_subscribe(Metric, DataPoints, _Interval, Opts, #state{subscriptions=Subscriptions} = State)
  when is_list(Opts) ->
    case proplists:get_value(path, Opts, undefined) of
        undefined ->
            {{error, path_missing}, State};
        Path ->
            BinPath = binarize_list(Path),
            Description = binarize_list(proplists:get_value(description, Opts, <<"">>)),
            Type = binarize_list(proplists:get_value(type, Opts, <<"">>)),
            case maps:is_key(BinPath, Subscriptions) of
                true ->
                    {ok, State};
                false ->
                    DataPoints1 = case is_list(DataPoints) of
                                      true   -> DataPoints;
                                      false  -> [DataPoints]
                                  end,
                    NewSubscriptions = maps:put(binarize_list(Path),
                                                {Metric, DataPoints1, Description, Type},
                                                Subscriptions),
                    {ok, State#state{subscriptions=NewSubscriptions}}
            end
    end;
exometer_subscribe(_Metric, _DataPoint, _Interval, _Opts, State) ->
    {{error, invalid_options}, State}.

exometer_unsubscribe(Metric, _DataPoint, _Extra, #state{subscriptions=Subscriptions} = State) ->
    Pred = fun(_Key, Value) ->
                   case Value of
                       {Metric, _, _, _} -> true;
                       _                 -> false
                   end
           end,
    PathToDelete = hd(maps:keys(maps:filter(Pred, Subscriptions))),
    NewSubscriptions = maps:remove(PathToDelete, Subscriptions),
    {ok, State#state{subscriptions=NewSubscriptions}}.

exometer_call({request, Path, DataPoint}, _From, #state{subscriptions=Subscriptions} = State) ->
    {reply, get_metrics(Path, format_datapoint(DataPoint), Subscriptions), State};
exometer_call(_Req, _From, State) ->
    {ok, State}.

exometer_newentry(#exometer_entry{name = Name, type = Type}, 
                  #state{autosubscribe = Autosubscribe, 
                         subscriptions_module = Module} = State) ->
    case {Autosubscribe, Module} of
        {true, Module} when is_atom(Module); Module /= undefined ->
            subscribe(Module:subscribe(Name, Type));
        _ -> []
    end,
    {ok, State}.

exometer_report(_Metric, _DataPoint, _Extra, _Value, State) -> {ok, State}.
exometer_cast(_Unknown, State) -> {ok, State}.
exometer_info(_Info, State) -> {ok, State}.
exometer_setopts(_Metric, _Options, _Status, State) -> {ok, State}.
exometer_terminate(_Reason, _) -> ignore.


%% ===================================================================
%% Internal functions
%% ===================================================================
subscribe(Subscriptions) when is_list(Subscriptions) ->
    [subscribe(Subscription) || Subscription <- Subscriptions];
subscribe({Name, DataPoint, Interval, Extra}) ->
    exometer_report:subscribe(?MODULE, Name, DataPoint, Interval, Extra, false);
subscribe(_Name) -> [].

binarize_list(List) when is_list(List) -> list_to_binary(List);
binarize_list(Term) -> Term.

format_datapoint(Binary) when is_binary(Binary) ->
    case re:run(Binary, "^.[0-9]*$") of
        {match, _} -> binary_to_integer(Binary);
        _NoMatch   -> binary_to_atom(Binary, latin1)
    end;
format_datapoint(Atom) -> Atom.

get_metrics(Path, DataPoint, Subscriptions) ->
    case maps:get(Path, Subscriptions, undefined) of
        undefined when DataPoint =:= undefined ->
            % return all metrics which habe `Path` as prefix in their path,
            % the path of each metric is included in the JSON object
            find_metrics(Path, Subscriptions);
        undefined ->
            % when a datapoint is given in the http params we require the
            % url to match a configuered path of a metric
            {error, not_found};
        MetricInfo when DataPoint =:= undefined ->
            % return this single metric without its path in the JSON object;
            % since no datapoint is defined all of them will be considered
            proceed_single_metric(Path, MetricInfo, true, false);
        {Metric, _DataPoints, Description, Type} ->
            % return the given single datapoint of the found metric;
            NewMetricInfo = {Metric, [DataPoint], Description, Type},
            case proceed_single_metric(Path, NewMetricInfo, false, false) of
                {ok, [{_, FinalPayload}]} -> {ok, FinalPayload};
                _Error -> {error, not_found}
            end
    end.

proceed_single_metric(Path, {Metric, DataPoints, Description, Type}, InfoFlag, PathFlag) ->
    case exometer:get_value(Metric, DataPoints) of
        {ok, DataPointValues} ->
            DataPoints1 = [{DataPoint, binarize_list(Value)} ||
                           {DataPoint, Value} <- DataPointValues],
            Payload = maybe_add_more(Path, DataPoints1, Description,
                                     Type, InfoFlag, PathFlag),
            {ok, Payload};
        _Error ->
            {error, not_found}
    end.

maybe_add_more(_Path, DataPoints, _Description, _Type, false, _Pathflag) -> DataPoints;
maybe_add_more(_Path, DataPoints, Description, Type, true, false) ->
    [{description, Description}, {type, Type}, {datapoints, DataPoints}];
maybe_add_more(Path, DataPoints, Description, Type, true, true) ->
    [{path, Path}, {description, Description}, {type, Type}, {datapoints, DataPoints}].

find_metrics(Path, Subscriptions) ->
    PrefixPaths = get_prefix_paths(Path, maps:keys(Subscriptions)),
    Pred = fun(K,_V) -> lists:member(K, PrefixPaths) end,
    FilteredSubsciptions = maps:filter(Pred, Subscriptions),
    case accumulate_metrics(maps:to_list(FilteredSubsciptions)) of
        []      -> {error, not_found};
        Metrics -> {ok, Metrics}
    end.

get_prefix_paths(Path, Paths) ->
    ByteSize = byte_size(Path),
    [ SinglePath || SinglePath <- Paths, ByteSize==binary:longest_common_prefix([Path, SinglePath]) ].

accumulate_metrics(Subscriptions) ->
    accumulate_metrics(Subscriptions, []).

accumulate_metrics([], Metrics) ->
    Metrics;
accumulate_metrics([{Path, MetricInfo} | Subscriptions], Metrics) ->
    case proceed_single_metric(Path, MetricInfo, true, true) of
        {ok, Metric}     -> accumulate_metrics(Subscriptions, Metrics ++ [Metric]);
        {error, _Reason} -> accumulate_metrics(Subscriptions, Metrics)
    end.

