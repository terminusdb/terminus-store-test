:- module(run_test, [go/0]).
/** <module> Load test program for terminus_store
 *
 * Usage:
 *
 * ==
 * cd terminus_store_test/prolog
 * swipl run_test.pl
 * ?-go.
 * ==
 *
 *
 */

:- use_module(library(optparse)).
:- use_module(test_data).
:- use_module(do_tests).

ok_version(80003).

%!  go is det
%
%   Repeatedly runs the load tests
%   recording data.
%
go :-
    current_prolog_flag(version, V),
    (   ok_version(V)
    ;   ok_version(OK),
        format(atom(Msg), 'You must be using SWI-Prolog ~w', [OK]),
        throw(error(domain_error(OK, V),
                    context(run_test:go/0, Msg)))
    ),
    opt_spec(OptsSpec),
    current_prolog_flag(argv, Args),
    opt_parse(OptsSpec, Args, Opts, _PositionalArgs, [duplicated_flags(keepall)]),
    writeln(Opts),
    (   memberchk(help(true), Opts)
    ->  opt_help(OptsSpec, HelpStr),
        print_message(help , help_info(HelpStr)),
        halt
    ;
        do_tests(Opts)
    ).

:- multifile
        prolog:message//1.

prolog:message(help_info(HelpStr)) -->
        [ '~w'-[HelpStr], nl ].

%!  do_tests(+Opts:list) is det
%
%   do the tests
%
%   @arg Opts the options list from cmd line
%
do_tests(Opts) :-
    memberchk(count(N), Opts),
    setup_call_catcher_cleanup(
        setup_tests(Opts, OutStream),
        do_n_tests(N),
        Catcher,
        end_tests(Catcher, OutStream)
    ).

end_tests(exit, OutStream) :-
    close(OutStream).
end_tests(fail, OutStream) :-
    writeln(OutStream, 'test failed'),
    close(OutStream).
end_tests('!', OutStream) :-
    close(OutStream).
end_tests(exception(E), OutStream) :-
    writeln(OutStream, 'threw exception'),
    format(OutStream, '~q~n', [E]),
    close(OutStream).
end_tests(external_exception(E), OutStream) :-
    writeln(OutStream, 'threw exception'),
    format(OutStream, '~q~n', [E]),
    close(OutStream).

%!  setup_tests(+Opts:list, -OutStream:term) is semidet
%
%   Setup to start running tests
%
%   @arg Opts the command line arguments
%   @arg OutStream the file to append csv rows to
%
%  This sets the b_setvals csv_stream and cmd_line_opts
%
setup_tests(Opts, OutStream) :-
    memberchk(settings(SettingsFileName), Opts),
    load_settings(SettingsFileName),
    open_csv_file(Opts, OutStream),
    b_setval(csv_stream, OutStream),
    b_setval(cmd_line_opts, Opts).

%!  opt_spec(-Spec:list) is det
%
%   binds Spec to the command line argument spec
%
opt_spec([
    [opt(settings), type(atom), default('settings.db'),
    shortflags([s]), longflags([settings]),
     help(['location of settings file'])],
    [opt(count), type(integer), default(-1),
     shortflags([n]), longflags([count]),
     help(['number of iterations to run (default -1, forever)'])],
    [opt(datafile), type(atom), default('loaddata.csv'),
     longflags([csv]),
     help(['path to the output data file. if it exists we append'])],
    [opt(omit), type(atom), longflags([omit]), default(no_such_test_exists),
     help(['omit a test- --omit failing_functor . May be repeated.'])],
    [opt(help), type(boolean), default(false),
     shortflags([h]), longflags([help]),
     help(['print help and exit'])]
]).
