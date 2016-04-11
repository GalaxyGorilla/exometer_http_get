# exometer_http_get [![Build Status](https://travis-ci.org/GalaxyGorilla/exometer_http_get.svg)](https://travis-ci.org/GalaxyGorilla/exometer_http_get)

This reporter makes metrics from exometer available via HTTP GET returning data as plain text or JSON.

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
        ]}
    ]}
}.
```

Available options:

* __host__ - Host IP. `{127, 0, 0, 1}` by default.
* __port__ - Host port. `8080` by default.
* __autosubscribe__ - Enables automatic subscriptions via a callback module. `false` by default.
* __subscriptions_module__ - Callback module for automatic subscriptions.


### Subscription examples:

In application config:

```erlang
{exometer,
    {subscriptions, [
        {exometer_report_http_get, [erlang, memory], total, manual,
         [{path, "/erlang/memory"}, {description, "Memory usage of BEAM"}, {type, "byte"}]},
    ]}
}.
```

As function call within your modules:

```erlang
exometer_report:subscribe(exometer_report_http_get, [erlang, memory], total, manual,
         [{path, "/erlang/memory"}, {description, "Memory usage of BEAM"}, {type, "byte"}]).
```

Check if everything is working:

```
curl -i http://localhost:8080/erlang/memory
```

By default JSON is returned. However, if you want plain text just set the accept header to `text/plain`.

Of course you can also check it in your browser. It is highly recommended to use JSON support if you use the JSON output, e.g. the
[JSON View extension](https://chrome.google.com/webstore/detail/jsonview/chklaanhfefbnpoihckbnefhakgolnmc?hl=en) or similar to make the metrics browsable.
JSON output example:

```
{
    description: "Memory usage of BEAM",
    type: "byte",
    datapoints: {
        total: 32647912
    }
}
```

Plain text output example:

```
description="Memory usage of BEAM" type="byte" datapoints=[ total=32647912 ]
```

Subpaths are possible, hence you can query `http://localhost:8080/erlang` which will retrieve all metrics which have `/erlang` as prefix.
Additionally the complete URL of every metric will be part of each JSON or text such that it is easy to find them. For the plain text output each metric is
printed in a single line, for JSON you get an array of metric objects.

The `description` and `type` fields are not displayed if their values are not present in the subscription call or empty!

Also datapoints can be provided as http parameter, e.g. `http://localhost:8080/erlang/memory?datapoint=total`, such that the corresponding metric value is
returned without any further text or JSON structure around it. This makes scripting with e.g. curl for monitoring purposes very easy.

#### About exometer subscription time intervals:

The report interval in subscriptions should be set to `manual` such that the metric is actually never reported using time interval triggers.
The metric value will be retrieved from exometer directly when one sends a HTTP GET request to the corresponding URL.

### Auto subscriptions:

It is possible to create a subscription automatically for each newly created metric entry. By default this is disabled. You can enable it in the reporter options.
You must also provide a callback module which handles the entries.

```erlang
{exometer,
    {reporters, [
        {exometer_report_http_get, [
            {autosubscribe, true},
            {subscriptions_module, exometer_http_get_subscribe_mod},
            {host, {127, 0, 0, 1}},
            {port, 8080}
        ]}
    ]}
}.
```

The callback module may look like:

```erlang
-module(exometer_http_get_subscribe_mod).
-export([subscribe/2]).

subscribe([test, metric], histogram) ->
    {[test, metric], [max, min], [{path, "/some/path"}]};
subscribe(_, _) -> [].
```

`subscribe/2` calls for each new entry and it should return a (possibly empty) list or just one subscription. Here a single subscription has the following layout:

```erlang
{exometer_report:metric(), exometer_report:datapoints(), exometer_report:extra()}
```
