#!/bin/bash

cd test && emacs -batch -l ert -l test.el -f ert-run-tests-batch-and-exit
