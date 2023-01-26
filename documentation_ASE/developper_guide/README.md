Developer guide to BINSEC/ASE
===

# General information

All modifications to the BINSEC SE engine are in the `src/ee` folder. The rest of the tool has been kept as is.


# Fault injection configuration options

Options have been added to the standard SE options in the files `sse_options.ml` and `.mli`. They cover all the configurable attacker behaviors and ASE dedicated options.


# Guides overview

We provide a few different guides to help understand our main modifications to the original BINSEC SE engine.
- [core_concepts.md](./core_concepts.md) provides details on major type changes made.
- [se_modifs.md](./se_modifs.md) details changes to the original symbolic execution engine
- [solver_interactions.md](./solver_interactions.md) details modifications made in the interaction with the solver.
- [adding_fm.md](./adding_fm.md) gives some advice on how to add your own custom fault model.