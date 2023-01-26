Core implementation concepts
===

# Dual symbolic state

To implement the IOD optimization, we transformed the symbolic state into a dual symbolic state, of type `dual` (defined in `senv.ml`). It is composed of the `normal_state` and the `faulted_state`. 
In order to have our various optimizations working on the same code, the symbolic state is replaced by a dual state everywhere, for all injection strategies. If the `InjectionMethod` is None, only the `normal_state` is used, if something else, other than IOD, we use only the faulted state only. And with IOD, both are used.
This is defined in the `Env` module (in `sse.ml`) by functions `main_predicate` and `secondary_predicate`.

# Additions to the path predicate

The path predicate is defined in the `Path_state` module of the `sse_types.ml` file.

Main modifications:
- `predicate_flips` is a list of conditional jump addresses, along with a character designing the branch where a switch happened in IOD mode.
- `max_fault_reached` is a flag indicating if we known, by any technique, that we reached the maximum number of faults on a path. It is used to stop the injection further down that path.
- `first_faults` is used to determined if faults have already been introduced in the path, or if the dedicated machinery can be skipped for performance.
- `under_approx_injections` is the under-approximation of the fault counter used in IOD, counting the path predicate switches.
- `depth_bcht` and `injection_points_in_path` are used to compute statistic at the path level, for the number of branchement and number of fault injections respectively.

Useful function manipulating those fields are also defined in the same file.