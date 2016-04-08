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
            Description = binarize_list(proplists:get_value(description, Opts, undefined)),
            Type = binarize_list(proplists:get_value(type, Opts, undefined)),
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

exometer_call({path, Path}, _From, #state{subscriptions=Subscriptions} = State) ->
    case maps:get(Path, Subscriptions, undefined) of
        undefined ->
            {reply, {error, not_subscribed}, State};
        {Metric, DataPoints, Description, Type} ->
            case exometer:get_value(Metric, DataPoints) of
                {ok, DataPointValues} ->
                    DataPoints1 = [{DataPoint, binarize_list(Value)} ||
                                   {DataPoint, Value} <- DataPointValues],
                    MetricInfo  = [{description, Description},
                                   {type, Type},
                                   {datapoints, DataPoints1}],
                    {reply, {ok, MetricInfo}, State};
                _Error -> {reply, {error, not_found}, State}
            end
    end;
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
subscribe(Subscribtions) when is_list(Subscribtions) ->
    [subscribe(Subscribtion) || Subscribtion <- Subscribtions];
subscribe({Name, DataPoint, Interval, Extra}) ->
    exometer_report:subscribe(?MODULE, Name, DataPoint, Interval, Extra, false);
subscribe(_Name) -> [].

binarize_list(List) when is_list(List) -> list_to_binary(List);
binarize_list(Term) -> Term.
