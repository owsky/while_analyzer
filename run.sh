#! /bin/bash
cabal build -v0 
# ./cabal_run.sh while-analyzer-exec "$@" --m -10 --n 10 --abstract_domain interval
./cabal_run.sh while-analyzer-exec "$@" --m -inf --n +inf --widening_delay 1 --descending_steps 2 --abstract_domain interval