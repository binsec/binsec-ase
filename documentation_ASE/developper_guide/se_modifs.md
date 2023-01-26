Modifications to the SE engine
===

The SE engine is in the `sse.ml` file. We present here the main modifications we made to it in order to include fault injection.


# SE Initialization

The initialization is in the `do_sse` function. Note that fault injection is not available with starting from a core dump.

Modifications to the SE initialization include:
- Initialization of the fault counter `NUMBER_FAULTS`, the maximum number of faults `MAX_FAULTS`, the accumulator of constrains of faulted values (different from the original value for instance) `FAULT_CONSTRAINS`.
- Computation of the reach condition: number of fault not exceeding the maximum and the constraint on faults satisfied.
- Adding the reach directive corresponding to the `GoalAddress` option, and the cut directive corresponding to the `AssertFailAddress` option.
- Detect if the attacker model cannot perform faults, in which case a flag `reaching_max_fault` is raised in the path predicate.

**Warning**: the initialization contains hardcoded variable length, set to 32 bits.


# SE instruction handling

The recognition of instruction and dispatch to the appropriate functions is in the `go` function. We only modified the `assignment` function handling `Assign` and the `ite` function handling `If`, i.e. conditional jumps.

## Assign

The original SE engine separated the assignment process according to the type of the LValue (`Var`, `Restrict` or `Store`). We encapsulated each behavior in a function, `assignment_lvalue` and `assignment_store`, that we pass as argument to our fault handling process called `dual_assign`.

The `dual_assign` separate according to the `InjectionMethod` option. For `none` and `test-inversion`, no faults are performed. 
Otherwise, a filtering function `is_fault_location` is applied. It checks:
- the flags stopping the injection
- that the address is in the target range
- doesn't fault temporary variables (dba internal variables), and accepts fault on flags according to the `FaultFlags` option.
- if the RValue is a constant above a threshold defined in the `ValueThresholdForFaults` option (default 0x500000000), identifying it as an address, which we don't fault
- if the LValue contains a blacklisted variable, defined in the `TargetBlackList` option.

In case of the IOD option, the normal (non-faulted) assignment is done on the main path predicate, and if the filter passes, the fault is injected through a fault model wrapper, `faulty_assignment`, otherwise a normal assignment is performed on the faulty path predicate.
In case of `always` (i.e. data faults without IOD), only the last part is performed.

The `faulty_assignment` separates cases according to the fault model and dispatch to the appropriate function encoding the desired fault. Their names should be self-explanatory.

A typical branchless data fault starts to check whether the number of fault check should be performed at each fault location, `WhereCheckNbFaults` option at `loc`, and if so, does it.
Then idem for the `WhereCheckNonDetConstrain`. This may require to define the fresh value, i.e. the fault value, that is called `non_det`, and the boolean representing the activation of the fault, called `b`.
Finally, the fault counter is incremented by `b` and a new RValue is computed, embedding the fault. This RValue is returned and assigned.

A typical branching data fault encoding starts in the same way, with the `WhereCheckNbFaults` and `WhereCheckNonDetConstrain` checks. Here, the fresh boolean variable `b` serves to split the path in two, either performing a faulty assignment and incrementing the fault counter in the `consequent` branch or performing the normal assignment in the `alternative` branch. Both are then recorded in the worklist and one is chosen to keep exploring first with the `Env.pick_alternative` function. 

## Conditional jump

The conditional jump evaluation is separated according to the injection method. A computation for the conditional expression and query variations are performed in generic functions implementing the decision scheme for a branch. It is then used for the 'then' branch and the 'else' branch. 
The essential 'then' and 'else' branch behavior (what to do when feasibility is ok) is wrapped in functions `then_branch` and `else_branch`.
All possible path are added to the worklist.

For each generic function, checks based on the fault constrain and of the number of faults are computed and added to the `Senv.assume` call. Note that the condition will be added to the pat predicate, while the `faut_check` and `such_that` argument will be included in the query, but not in the path predicate going further. 

For IOD, or `on-demand`, the generic scheme is to first assess the feasibility of the branch on the normal path predicate (with minimal faults). Then if it is not feasible, to try it with the faulted predicate if we didn't reach the maximum number of switches. Note that the combination of IOD and EDS is also implemented here, checking the faulted path with (nb_f < max_f) then (nb_f == max_f). 

The `always` injection method regroup no optimization and EDS. the generic branch functions behave in a similar manner.

Last, the `test-inversion` injection method is used to invert tests. As for data faults, a filtering function `is_fault_location` is first applied, checking no stopping flags have been raised, that we are in the target range and that the expression doesn't contain blacklisted variables.
Then, cases are separated according to the exact test inversion fault model. The condition will be faulted exactly like a data fault, and the new expression of the condition will be given to a generic branch function.



# SE reach directives handling

We modified the `handle_reach` function to include the IOD logic, first checking on the main path predicate, and then on the faulted one with more faults can be added.
In addition, all reach queries are augmented with a number of fault check and a fault constrain check.