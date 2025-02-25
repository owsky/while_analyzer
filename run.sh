#! /bin/bash
cabal build -v0 
./cabal_run.sh while-analyzer-exec "$@" --m -inf --n +inf --widening_delay 0 --descending_steps 1 --abstract_domain interval