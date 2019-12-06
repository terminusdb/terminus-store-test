:- module(proc_data, [
              proc_datum/1
          ]).
/** <module> Data gathering from the /proc file system
 *
 */

user:file_search_path(proc, '/proc').

%!  proc_datum(-Datum:datum) is nondet
%
%   enumerate the values in /proc/PID/stat as datum dicts
%
proc_datum(Datum) :-
    current_prolog_flag(pid, PID),
    proc_filenames(FileNames),
    member(FileName, FileNames),
    read_file_to_string(proc(PID/FileName), Contents, []),
    split_string(Contents, " ", " ", StatStrs),
    proc_file_fields(stat, Fields),
    maplist(as_datum, Fields, StatStrs, Data),
    member(Datum, Data),
    Datum.name \= placeholder.

as_datum(Name, Val, datum{name:Name, val:Val}).

%!  proc_file_fields(+Name:atom, -Fields:list) is det
%
%   list of fields in a proc filesystem file
proc_file_fields(Name, Fields) :-
    proc_sys_fields(PSF),
    memberchk(Name-Fields, PSF).

proc_filenames([stat]).

proc_sys_fields([
    stat-
    [
pid,
tcomm,
state,
ppid,
pgrp,
sid,
tty_nr,
tty_pgrp,
flags,
min_flt,
cmin_flt,
maj_flt,
cmaj_flt,
utime,
stime,
cutime,
cstime,
priority,
nice,
num_threads,
it_real_value,
start_time,
vsize,
rss,
rsslim,
start_code,
end_code,
start_stack,
esp,
eip,
pending,
blocked,
sigign,
sigcatch,
placeholder,
placeholder,
placeholder,
exit_signal,
task_cpu,
rt_priority,
policy,
blkio_ticks,
gtime,
cgtime,
start_data,
end_data,
start_brk,
arg_start,
arg_end,
env_start,
env_end,
exit_code
    ]
]).

