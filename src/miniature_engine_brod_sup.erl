-module(miniature_engine_brod_sup).

-export([
    start_link/1
]).

-behaviour(supervisor_bridge).

-export([
    init/1,
    terminate/2
]).

start_link(ClientId) ->
    supervisor_bridge:start_link(?MODULE, ClientId).

init(ClientId) ->
    ok = brod:start_client(endpoints(), ClientId, [
        {auto_start_producers, true}
    ]),
    Pid = whereis(ClientId),
    {ok, Pid, state()}.

terminate(_Reason, _State) ->
    ok.

%% private
endpoints() ->
    {ok, EndpointsString} = application:get_env(kafka_endpoints),
    endpoints(EndpointsString).

endpoints(EndpointsString) when is_binary(EndpointsString) ->
    endpoints(binary_to_list(EndpointsString));
endpoints(EndpointsString) ->
    lists:map(fun(Endpoint) ->
        [Host, Port] = string:split(Endpoint, ":"),
        {Host, list_to_integer(Port)}
    end, string:split(EndpointsString, ",", all)).

%% private
state() ->
    {}.
