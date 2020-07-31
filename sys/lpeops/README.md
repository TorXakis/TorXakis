# lpeops

`lpeops` contains operations that can be applied to models that have been transformed to a Linear Process Equation (LPE) using the `lpe` command.
In fact, the operations make it possible to reduce the size of the LPE and/or the size of the corresponding state space.

The LPE operations are performed symbolically so that the reductions can be done without generating the state space first.
Several of the operations are based on the techniques that have already been implemented in the [mCRL2](https://www.mcrl2.org) toolset.
The operations can be applied in a loop (until a fixpoint is reached) because every change to the LPE may allow further reductions.

