# Squanto, an quantum optimizer in SML
This project builds on top of the library [Quantum Circuits and Simulation in Standard ML for the ATPL MSc Course](https://github.com/diku-dk/atpl-sml-quantum)'
by Martin Elsman 

## Dependencies
Because of this we require all the packages that the library does, such as MLKit and and smlpkg.
To install these please follow the guide in the README, in the inner repo

## Setting up
Before trying to compile the tests and benchmarks, please make sure to be in the atpl-sml-quantum/ directory 
and run the commandos 
```
$ make test
$ smlpkg sync
```

## Code
Our projec implementation can be found in the atpl-sml-quantum/src/optimizer/ directory, where you will find 6 major files 
* qGenerator.sig   : the signature file for the circuit identity generator
* qGenerator.sml   : the implementation of the generator
* optimizer.sig    : the signature file of the circuit optimizer
* optimizer.sml    : the implementation of the optimizer
* main.sml         : the unit tests
* run_mlb_files.sh : script to run the benchmarks

To run the tests compile the main.mlb file and run the executable "rub" (making sure to be in the ../optimizer/ directory
```
$ mlkit main.mlb
$ ./run
```
To run the benchmarks simply run the bash script
```
$ bash run_mlb_files.sh
```
note that some of the benchmarks crashes intentionally due to seg-fault
