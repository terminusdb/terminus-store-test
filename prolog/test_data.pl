:- module(test_data, [
              write_csv_header/1,
              write_csv_row/1,
              open_csv_file/2,
              collect_data/1
          ]).
/** <module> Utilities to write TerminusDB test data  to a CSV file
 *
 */
:- use_module(library(csv)).
:- use_module(proc_data).

%!  open_csv_file(+Opts:list, -OutStream:stream) is det
%
%   Open the csv file and ready it for tests
%
%   @arg Opts command line options
%   @arg OutStream csv file stream to  write to
%
open_csv_file(Opts, OutStream) :-
    memberchk(datafile(OutFile), Opts),
    (   exists_file(OutFile)
    ->
        open(OutFile, append, OutStream)
    ;
        open(OutFile, write, OutStream),
        write_csv_header(OutStream)
    ).

%!  write_csv_header(+Stream:stream) is det
%
%   @arg Stream the stream to write to
%
write_csv_header(Stream) :-
    csv_data_field_names(Names),
    Row =.. [row | Names],
    csv_write_stream(Stream, [Row], []).

write_csv_row(Data) :-
    Row =.. [row | Data],
    b_getval(csv_stream, Stream),
    csv_write_stream(Stream, [Row], []),
    flush_output(Stream).

csv_data_field_names(List) :-
    bagof(X , datum(X), Tests),
    maplist(get_dict(name), Tests, List).

datum(datum{ name: utime, val: UTime}) :-
    get_time(UTime).
datum(datum{ name: Name, val: Val}) :-
    member(Stack, [local, global, trail]),
    member(Goal, [min_free(Val), low(Val), factor(Val)]),
    prolog_stack_property(Stack, Goal),
    Goal =.. [Functor | _],
    atomic_list_concat([Stack, ' ', Functor], Name).
datum(Datum) :-
    proc_datum(Datum).


collect_data(List) :-
    bagof(X , datum(X), Tests),
    maplist(get_dict(val), Tests, List).
