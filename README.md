# terminus-store load testing application

[![Build Status](https://travis-ci.com/terminusdb/terminus_store_test.svg?branch=master)](https://travis-ci.com/terminusdb/terminus_store_test)

Prolog bindings for the [terminus-store](https://github.com/terminusdb/terminus-store/) Rust library.

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

## Compiling and running

```
cd terminus_store_test/prolog
swipl run_test.pl -- -n 300
```

See the built in help


```
cd terminus_store_test/prolog
swipl run_test.pl -- --help
```
