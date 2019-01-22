%%%-------------------------------------------------------------------
%% @doc miniature_engine top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(miniature_engine_sup).

-export([
    start_link/0
]).

-behaviour(supervisor).

-export([
    init/1
]).

-define(SERVER, ?MODULE).

-define(FLAGS, #{
    strategy  => one_for_one,
    intensity => 5,
    period    => 10
}).

-define(CHILD(Id, Mod, Args), #{
    id       => Id,
    start    => {Mod, start_link, Args},
    restart  => permanent,
    shutdown => 5000,
    type     => supervisor,
    modules  => [Mod]
}).

-define(CHILD(I), ?CHILD(I, I, [])).

-define(CHILDREN, [
    ?CHILD(miniature_engine_cowboy_sup),
    ?CHILD(miniature_engine_producer_sup, miniature_engine_brod_sup, [miniature_engine_producer]),
    ?CHILD(miniature_engine_consumer_sup, miniature_engine_brod_sup, [miniature_engine_consumer]),
    ?CHILD(miniature_engine_subscriber_sup)
]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {?FLAGS, ?CHILDREN}}.
