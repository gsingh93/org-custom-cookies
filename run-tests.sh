#!/bin/bash

cd test && emacs -q -batch -l ert -l test.el -f ert-run-tests-batch-and-exit
