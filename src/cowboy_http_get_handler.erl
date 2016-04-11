-module(cowboy_http_get_handler).

-export([init/3,
         resource_exists/2,
         allowed_methods/2,
         content_types_provided/2]).

-export([get_json/2,
         get_plain_text/2]).

%% ===================================================================
%% cowboy callbacks
%% ===================================================================
init(_Type, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    Provided = [{{<<"application">>, <<"json">>, []}, get_json},
                {{<<"text">>, <<"plain">>, []}, get_plain_text}],
    {Provided, Req, State}.

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

get_json(Req, Payload) ->
    Json = jsx:encode(Payload),
    StrippedJson = re:replace(Json, ["\\\\"], "", [{return, binary}, global]),
    {StrippedJson, Req, []}.

get_plain_text(Req, Payload) when is_list(Payload) ->
    PlainText = encode_plain_text(Payload),
    {PlainText, Req, []};
get_plain_text(Req, Payload) ->
    {make_io(Payload), Req, []}.

encode_plain_text(Payload) ->
    FirstLevel = get_first_level(Payload),
    [ [encode_plain_text(Term, []), "\n"] || Term <- FirstLevel ].

get_first_level(Payload = [Content|_]) when is_tuple(Content) -> [Payload];
get_first_level(Payload) -> Payload.

encode_plain_text({datapoints, V}, Akk) ->
    Akk ++ [make_io(datapoints), "=[ ",  encode_plain_text(V, []), "]"];
encode_plain_text({K, V}, Akk) ->
    Akk ++ [make_io(K), "=",  encode_plain_text(V, []), " "];
encode_plain_text(Payload = [{_K, _V}|_], Akk) ->
    Akk ++ [encode_plain_text(Elem, []) || Elem <- Payload];
encode_plain_text(Term, Akk) ->
    Akk ++ [make_io(Term)].

make_io(Atom) when is_atom(Atom) -> atom_to_list(Atom);
make_io(Integer) when is_integer(Integer) -> [integer_to_list(Integer)];
make_io(Term) -> Term.

