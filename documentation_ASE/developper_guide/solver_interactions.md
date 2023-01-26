Interactions with the solver
===

Fault injection is compatible with any solver BINSEC supports, including the native binding with Bitwuzla.

The interactions with the solver go throught the `senv.ml file.

The main modifications made are:
- adapting to the dual symbolic state
- including `fault_check` and `such_that` arguments in the `assume` function, adding constrains in the query but not in the path predicate.
- machinery to avoid faulting adresses, in particular in assignments of Load terms (`assign`) and the store of value (`write`). This is pattern-based and set faults to zero (not active) if matched. 

Note that you might come across 'sfs' references. It stands for sub-fault simplification and was implemented with various strategies but didn't improved performance, so support was abandoned. 