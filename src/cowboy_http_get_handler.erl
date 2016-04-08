-module(cowboy_http_get_handler).

-export([init/3,
         resource_exists/2,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2]).

-export([get_json/2, put_json/2]).

%% -- cowboy callbacks
init(_Type, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

resource_exists(Req, _State) ->
    {Path, Req1} = cowboy_req:path(Req),
    case get_metric_info(Path) of
        {ok, Payload} ->
            {true, Req1, Payload};
        {error, _Reason} ->
            {halt, Req1, []}
    end.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, put_json}], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.


%% -- Helpers
get_metric_info(Path) ->
    exometer_report:call_reporter(exometer_report_http_get, {path, Path}).

put_json(Req, State) ->
    {true, Req, State}.

get_json(Req, Payload) ->
    Json = jsx:encode(Payload),
    StrippedJson = re:replace(Json, ["\\\\"], "", [{return, binary}, global]),
    {StrippedJson, Req, []}.

