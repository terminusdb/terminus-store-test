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
do_n_tests(N) :-
    succ(NN, N),
    test_prep,
    do_a_test,
    collect_data(CSV),
    write_csv_row(CSV),
    do_n_tests(NN).


test_prep :-
    trim_stacks,
    garbage_collect,
    garbage_collect_atoms,
    garbage_collect_clauses.

do_a_test :-
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
    triple(Layer, cow, says, value(moo)).

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

