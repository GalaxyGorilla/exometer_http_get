-module(cowboy_http_get_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Path, Req1} = cowboy_req:path(Req),
    case exometer_report:call_reporter(exometer_report_http_get, {path, Path}) of
        {ok, Value} ->
            StringDatapoints = lists:flatten(io_lib:format("~p",[Value])),
            {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], list_to_binary(StringDatapoints), Req1);
        {error, _Reason} ->
            {ok, Req2} = cowboy_req:reply(204, [{<<"content-type">>, <<"text/plain">>}], <<"">>, Req1)
    end,
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) -> ok.
