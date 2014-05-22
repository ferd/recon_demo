-module(council_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, Names} = application:get_env(council, names),
    {ok, {{one_for_one, 1, 1},
            [{Name, {council_member, start_link, [Name, Names]},
              permanent, 5000, worker, [council_member]}
             || Name <- Names]}}.

