-module(gingko_bench_driver_counter_keys).
-behaviour(rcl_bench_driver).

-export([new/1, run/4, terminate/2]).

-export([mode/0, concurrent_workers/0, duration/0, operations/0, test_dir/0,
         key_generator/0, value_generator/0, random_algorithm/0,
         random_seed/0, shutdown_on_error/0]).

%% =======================
%% Benchmark configuration

mode() -> {ok, {rate, 5}}.
%% Number of concurrent workers
concurrent_workers() -> {ok, 5}.
%% Test duration (minutes)
duration() -> {ok, 30}.
%% Operations (and associated mix)
operations() ->
    {ok, [{get_version, 1}
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
  Key = Clock,
  Result = rpc:call(Node,Mod,get_version,[Key, antidote_crdt_counter_pn, vectorclock:set(mydc,Clock,vectorclock:new()), vectorclock:set(mydc,UpdatedClock,vectorclock:new())]),
  io:format("Worker: ~p Clock: ~p RPC Result: ~p ~n",[Id, Clock, Result]),
  {ok, State#state{clock = UpdatedClock}};

run(update, KeyGen, ValueGen, State = #state{node =Node, id = Id, module =Mod, clock = Clock}) ->
  io:format("Worker: ~p Clock: ~p Update ~n",[Id, Clock]),
  Key = counter_key,
  Result = rpc:call(Node,Mod,update,[Key, antidote_crdt_counter_pn,txnId, 1]),

  {ok, State};
run(commit, KeyGen, ValueGen, State = #state{node =Node,id = Id,  module =Mod, clock = Clock}) ->
  io:format("Worker: ~p Clock: ~p Commit ~n",[Id, Clock]),
  UpdatedClock = Clock,
  Key = counter_key,
  Result = rpc:call(Node,Mod,commit,[[Key],txnId,{1, 1234}, vectorclock:set(mydc,Clock, vectorclock:new())]),
  {ok, State#state{clock = UpdatedClock}}.

terminate(_, _) -> ok.