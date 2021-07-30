-module(gingko_bench_driver_skewed).
-behaviour(rcl_bench_driver).

-export([new/1, run/4, terminate/2]).

-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
         key_generator/0, value_generator/0, random_algorithm/0,
         random_seed/0, shutdown_on_error/0]).

%% =======================
%% Benchmark configuration

mode() -> {ok, {rate, max}}.
%% Number of concurrent workers
concurrent_workers() -> {ok, 1}.
%% Test duration (minutes)
duration() -> {ok, 2}.
%% Operations (and associated mix)
operations() ->
    {ok, [{get_version, 5},
      {update, 5},
      {commit, 5}
    ]}.

%% Base test output directory
test_dir() -> {ok, "tests"}.

%% Key generators
%% {uniform_int, N} - Choose a uniformly distributed integer between 0 and N
key_generator() -> {ok, {uniform_int, 1000}}.

%% Value generators
%% {fixed_bin, N} - Fixed size binary blob of N bytes
value_generator() -> {ok, {fixed_bin, 100}}.

random_algorithm() -> {ok, exsss}.
random_seed() -> {ok, {1,4,3}}.

shutdown_on_error() -> false.

gingko_ip() -> '127.0.0.1'.
gingko_type_operations() -> dict:from_list([{antidote_crdt_counter_pn, 1}]).

-record(state, {id, node, module, clock}).

%% ========================
%% Benchmark implementation

new(Id) ->
    Node = list_to_atom("gingko@127.0.0.1"),
    {ok, #state{id = Id, node = Node, module= gingko, clock = 0}}.

run(get_version, KeyGen, _ValueGen, State = #state{node =Node,id = Id, module =Mod, clock = Clock}) ->
  UpdatedClock = Clock+1,
  Result = rpc:call(Node,Mod,get_version,[dummy_key, antidote_crdt_counter_pn, vectorclock:set(mydc,UpdatedClock,vectorclock:new()), vectorclock:set(mydc,UpdatedClock+1,vectorclock:new())]),
  {ok, State#state{clock = UpdatedClock}};

run(update, KeyGen, ValueGen, State = #state{node =Node, id = Id, module =Mod, clock = Clock}) ->
  UpdatedClock = Clock+1,
  Result = rpc:call(Node,Mod,update,[dummy_key, antidote_crdt_counter_pn,txnId, 1]),
  {ok, State#state{clock= UpdatedClock}};
run(commit, KeyGen, ValueGen, State = #state{node =Node,id = Id,  module =Mod, clock = Clock}) ->
  UpdatedClock = Clock+1,
  Result = rpc:call(Node,Mod,commit,[[dummy_key],txnId,{1, 1234}, vectorclock:set(mydc,UpdatedClock, vectorclock:new())]),
  {ok, State#state{clock = UpdatedClock}}.

terminate(_, _) -> ok.