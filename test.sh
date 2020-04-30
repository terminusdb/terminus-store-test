#!/bin/sh

time sudo docker run --rm terminusdb/terminus-store-test:latest swipl -g go -g halt prolog/run_test.pl -- -n 1
