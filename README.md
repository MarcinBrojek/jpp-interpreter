# jpp-interpreter

#### LANGUAGE

An imperative language with syntax based on a subset of C/C++. It has been slightly modified to fulfill specific functionalities. Programs consist of variable/function declarations. The executed program will invoke the declared main function. More syntax details can be found in the language specification: Marcin_Brojek.md.

The provided interpreter adheres to the specifications outlined in the declaration, except for the incorrect handling of the get method for tuples.

#### OVERVIEW OF THE SOLUTION

The solution comprises two main modules: **TypeChecker** and **Interpreter**, both utilizing monads concurrently: Reader, State, Except. In the TypeChecker module, environment and state store type information, while in the Interpreter module, they store values.

Within a block - a sequence of instructions - variable declarations, functions, and procedures can appear at any position. Therefore, both modules above apply subsequent instructions to newly created, successive environments.

#### EXECUTION

Compiling using the **make** command will create a `build` directory and an executable `interpreter` file, which can be run with the command 
```
./interpreter program
```

#### EXAMPLES

In the `bad` directory, files are named `XX-YY-name-of-test.cpp`, where **XX** equals **00** signifies syntax errors, **10** denotes runtime errors caught dynamically, and **12** indicates errors during static type checking.

Files in the `good` directory are named `YY-name-of-test.cpp`, where **YY** corresponds to a functionality number from the Imperative Language list. Except for **99**, which points to other comprehensive program examples.

To run all examples based on the directory, you can use the command 
```
make good_run
``` 
or 
```
make bad_run
```
