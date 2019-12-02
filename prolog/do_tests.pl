:- module(do_tests, [
              do_n_tests/1
          ]).
/** <module> Do the actual tests
 *
 * master module for all tests
 */
:- use_module(test_data).
:- use_module(library(terminus_store)).

%!  do_n_tests(+N:integer, +Opts:list, +OutStream:stream) is det
%
%   @arg N number of tests to run, -1 is infinite number
%   @arg Opts command line options
%   @arg OutStream the stream to write data to
%
do_n_tests(0).
do_n_tests(-1) :-
    test_prep,
    do_a_test,
    collect_data(CSV),
    write_csv_row(CSV),
    do_n_tests(-1).
do_n_tests(N) :-
    succ(NN, N),
    test_prep,
    do_a_test,
    collect_data(CSV),
    write_csv_row(CSV),
    do_n_tests(NN).

%!  test_prep is det
%
%   clean up all memory prior to running a test
%
test_prep :-
    trim_stacks,
    garbage_collect,
    garbage_collect_atoms,
    garbage_collect_clauses.

%!  do_a_test is det
%
%   do a single test cycle
%
do_a_test :-
    debug(test(do_a_test), 'start test', []),
    % this test can't be omitted cause it inits the db
    open_directory_store('/tmp/demo', Store),
    uuid(UUID),
    atom_concat(mygraph, UUID, GraphName),
    create_named_graph(Store, GraphName, Graph),
    open_write(Store, Builder),
    nb_add_triple(Builder, cow, loves, node(duck)),
    nb_add_triple(Builder, duck, hates, node(cow)),
    nb_add_triple(Builder, cow, says, value(moo)),
    nb_commit(Builder, Layer),
    nb_set_head(Graph, Layer), % make graph point at layer
    triple(Layer, cow, loves, node(duck)),
    triple(Layer, duck, hates, node(cow)),
    triple(Layer, cow, says, value(moo)),
    opt_test(one_big_union_er_layer(Graph)),
    opt_test(zillions_o_layers(Graph)),
    debug(test(do_a_test), 'end of test', []).


:- meta_predicate opt_test(0).

%!  opt_test(+Goal:callable) is det
%
%   if this section is not omitted by the cmd line opts,
%   report in debug and do
%
opt_test(Goal) :-
    b_getval(cmd_line_opts, Opts),
    strip_module(Goal, Module, RawGoal),
    RawGoal =.. [Functor | _],
    (   memberchk(omit(Functor), Opts)
    ->
        debug(test(do_a_test), '~w:~w skipped', [Module, Functor])
    ;
        debug(test(do_a_test), 'starting ~w:~w', [Module, Functor]),
        call(Goal)
    ).


:- meta_predicate  between_map(+, +, 1).

between_map(From, To, _) :-
    From > To.
between_map(From, To, Goal) :-
    call(Goal, From),
    succ(From, NN),
    between_map(NN, To, Goal).

one_big_union_er_layer(Graph) :-
    head(Graph, OldLayer),
    open_write(OldLayer, Builder),
    between_map(1, 10_000, add_random_triple(Builder, onion)),
    nb_commit(Builder, Layer),
    nb_set_head(Graph, Layer),
    between_map(1, 10_000, random_triple(Layer, onion)).

zillions_o_layers(Graph) :-
    between_map(1, 10_000, add_random_triple_layer(Graph)),
    head(Graph, Layer),
    between_map(1, 10_000, random_triple(Layer, zillions)).

add_random_triple_layer(Graph, N) :-
    head(Graph, OldLayer),
    open_write(OldLayer, Builder),
    add_random_triple(Builder, zillions, N),
    nb_commit(Builder, Layer),
    nb_set_head(Graph, Layer).

add_random_triple(Builder, Base, N) :-
    atom_number(NA, N),
    atom_concat(Base, NA, Subj),
    Obj is random_float,
    number_string(Obj, SObj),
    debug(test(add_random_triple), '~w ~w ~w', [Subj, flamboglets, value(SObj)]),
    nb_add_triple(Builder, Subj, flamboglets, value(SObj)).

random_triple(Builder, Base, N) :-
    atom_number(NA, N),
    atom_concat(Base, NA, Subj),
    triple(Builder, Subj, flamboglets, value(_)).

collect_data([UTime,
              StackMinFree,
              StackLow,
              StackFactor,
              GlobMinFree,
              GlobLow,
              GlobFactor,
              TrailMinFree,
              TrailLow,
              TrailFactor]) :-
    get_time(UTime),
    prolog_stack_property(local, min_free(StackMinFree)),
    prolog_stack_property(local, low(StackLow)),
    prolog_stack_property(local, factor(StackFactor)),
    prolog_stack_property(global, min_free(GlobMinFree)),
    prolog_stack_property(global, low(GlobLow)),
    prolog_stack_property(global, factor(GlobFactor)),
    prolog_stack_property(trail, min_free(TrailMinFree)),
    prolog_stack_property(trail, low(TrailLow)),
    prolog_stack_property(trail, factor(TrailFactor)).

