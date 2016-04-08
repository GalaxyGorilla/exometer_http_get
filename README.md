# exometer_http_get [![Build Status](https://travis-ci.org/GalaxyGorilla/exometer_http_get.svg)](https://travis-ci.org/GalaxyGorilla/exometer_http_get)

This reporter makes metrics from exometer available via HTTP GET returning data as JSON Object.

### Usage

Add exometer_http_get to your list of dependencies in rebar.config:

```erlang
{deps, [
    {exometer_http_get, ".*", {git, "https://github.com/galaxygorilla/exometer_http_get.git", "master"}}
]}.
```

Ensure exometer_http_get is started before your application:

```erlang
{applications, [exometer_http_get]}.
```

Configure it:

```erlang
{exometer,
    {reporters, [
        {exometer_report_http_get, [
            {host, {127, 0, 0, 1}},
            {port, 8080}
        ]}]}
}.
```

Available options:

* __host__ - Host IP. `127.0.0.1` by default.
* __port__ - Host port. `8080` by default.
* __autosubscribe__ - Boolean. Enables automatic subscriptions via a callback module.
* __subscriptions_module__ - Callback module for automatic subscriptions.


### Subscription examples:

In application config:

```erlang
{exometer,
    {subscriptions, [
        {exometer_report_http_get, [erlang, memory], total, manual,
         [{path, "/erlang/memory"}, {description, "Memory usage of BEAM in bytes"}, {type, "gauge"}]},
    ]}
}.
```

As function call within your modules:

```erlang
exometer_report:subscribe(exometer_report_http_get, [erlang, memory], total, manual,
         [{path, "/erlang/memory"}, {description, "Memory usage of BEAM in bytes"}, {type, "gauge"}]).
```

Check if everything is working:

```
curl -i http://localhost:8080/erlang/memory
```

Of course you can also check it in your browser. It is highly recommended to use JSON support.

```json
{
    description: "Memory usage of BEAM in bytes",
    type: "gauge",
    datapoints: {
        total: 32647912
    }
}
```

Subpaths are also possible. Hence you can query `http://localhost:8080/erlang` which will retrieve all metrics which have `/erlang` as prefix. 
Additionally the complete path of every metric will be part of the JSON objects such that it is easy to find single metrics.

Note that the report interval should be set to `manual` such that the metric is actually never reported using the time triggers. The metric value will be retrieved from exometer directly when one sends a HTTP GET request to the URL.

### Auto subscriptions:

There is capability for making a subscription automatically for each new entry. By default it is off. If you need to enable it in the reporter options and also provide a callback module which handles newly created entries.

```erlang
{exometer,
    {reporters, [
        {exometer_report_http_get, [
            {autosubscribe, true},
            {subscriptions_module, exometer_http_get_subscribe_mod},
            {host, {127, 0, 0, 1}},
            {port, 8080}]
        }]}
}.
```

The callback module may look like:

```erlang
-module(exometer_http_get_subscribe_mod).
-export([subscribe/2]).

subscribe([metric, test], histogram) ->
    [{[metric, test], max, manual, [{path, "/some/path"}]},
     {[metric, test], median, manual, [{path, "some/other/path"}]}];
subscribe(_, _) -> [].
```

`subscribe/2` calls for each new entry and it should return a list or just one subscription. Here a single subscription has the following layout:
```erlang
{exometer_report:metric(), exometer_report:datapoint(), manual, exometer_report:extra()}
```
