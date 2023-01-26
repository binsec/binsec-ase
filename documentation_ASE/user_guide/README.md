# User guides

The following guides are available:
    - [configuration.md](configuration.md) details BINSEC/ASE configuration files linked with each benchmark.
    - [output.md](output.md) explains BINSEC/ASE output.
    - [new_setup.md](new_setup.md) provides information to setup a new benchmark.

# Getting started

Let's take `VerifyPIN_0` from the [FISSC benchmarks suite](https://lazart.gricad-pages.univ-grenoble-alpes.fr/fissc/) as an example for this guide. You will find the source code, binary executable and configuration file in examples/fault-injection/VerifyPIN_0.

To run the configuration attached to a benchmark, named `verifyPIN_0.cfg`, use the following command in the benchmark folder.
```
binsec -config verifyPIN_0.cfg
```
Note that there is only one '-' before option names, instead of the most common '--'.

If you wish to display the analysis time, we suggest using the command line:
```
time binsec -config verifyPIN_0.cfg
```

The output will tell you that for this attacker model, able to perform 1 arbitrary data fault in a Forkless manner, there are 3 attack paths (`Models found`). By going up the logs, you will get the details of the inputs and fault sequence generating those attack paths.