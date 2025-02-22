#! /bin/bash
cabal build -v0 
# ./cabal_run.sh while-analyzer-exec "$@" --m -inf --n +inf --widening_delay 0 --descending_steps 1 --abstract_domain interval
# ./cabal_run.sh while-analyzer-exec programs/divisionByZero.while --m -inf --n +inf --widening_delay 0 --descending_steps 1 --abstract_domain interval
# ./cabal_run.sh while-analyzer-exec programs/example.while --m -inf --n +inf --widening_delay 0 --descending_steps 1 --abstract_domain interval
# ./cabal_run.sh while-analyzer-exec programs/noInputState.while --m -inf --n +inf --widening_delay 0 --descending_steps 1 --abstract_domain interval
./cabal_run.sh while-analyzer-exec programs/deepNesting.while --m -inf --n +inf --widening_delay 0 --descending_steps 1 --abstract_domain interval