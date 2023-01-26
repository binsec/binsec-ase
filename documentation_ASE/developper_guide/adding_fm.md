Adding your own fault model
===

# Adding a data fault model

To add a new data fault model, for instance a set fault replacing a value by 0xffffffff:
- add the fault model name to the `FaultModel` option list in `sse_options.ml`.
- create your fault encoding function, based on the proposed ones, forking or forkless.
- add that function to the `faulty_assignment` match case.

And this is it !


# Other fault models

You can define other fault models by modifying the impacted part of the symbolic execution engine to obtain the desired effect. Note that it will probably be more complex and require a better understanding of BINSEC inner workings.