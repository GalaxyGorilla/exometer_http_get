-module(cowboy_http_get_handler).

-export([init/3,
         resource_exists/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2]).

-export([get_json/2, put_json/2]).

%% ===================================================================
%% cowboy callbacks
%% ===================================================================
init(_Type, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, put_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

resource_exists(Req, _State) ->
    {Path, Req1} = cowboy_req:path(Req),
    {HostUrl, Req2} = cowboy_req:host_url(Req1),
    {DataPoint, Req3} = cowboy_req:qs_val(<<"datapoint">>, Req2, <<"">>),
    case get_metric_info({HostUrl, Path}, DataPoint) of
        {ok, Payload} ->
            {true, Req3, Payload};
        {error, _Reason} ->
            {halt, Req3, []}
    end.
    

%% ===================================================================
%% helpers
%% ===================================================================
get_metric_info(Url, DataPoint) ->
    exometer_report:call_reporter(exometer_report_http_get, {request, Url, DataPoint}).

put_json(Req, State) ->
    {true, Req, State}.

get_json(Req, Payload) ->
    Json = jsx:encode(Payload),
    StrippedJson = re:replace(Json, ["\\\\"], "", [{return, binary}, global]),
    {StrippedJson, Req, []}.

