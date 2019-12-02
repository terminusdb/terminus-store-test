# terminus-store load testing application

[![Build Status](https://travis-ci.com/terminusdb/terminus_store_test.svg?branch=master)](https://travis-ci.com/terminusdb/terminus_store_test)

## Requirements

* cargo
* gcc
* swi-prolog 8.0.3 (with the include headers)
* terminus_store_prolog
* terminus_store

## Installing

```
?- pack_install(terminus_store_prolog).
```

You will need to make the directory /tmp/demo if it doesn't exist

## Compiling and running

```
cd terminus_store_test/prolog
swipl run_test.pl -- -n 1
?- debug(test(_)).
?- go.
```

The run should be appended to the file `prolog/loaddata.csv`.

See the built in help for options.

```
cd terminus_store_test/prolog
swipl run_test.pl -g "go" -- --help
```
Currently it's

```
--settings  -s  atom=settings.db          location of settings file
--count     -n  integer=-1                number of iterations to run (default -1, forever)
--csv           atom=loaddata.csv         path to the output data file. if it exists we append
--omit          atom=no_such_test_exists  omit a test- --omit failing_functor . May be repeated.
--help      -h  boolean=false             print help and exit
```

