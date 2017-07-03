# abstract-machine

This project contains step-by-step execution for three execution models:
 1. Abstract Machine (`abstract-machine-app` folder)
 2. Big Step (`big-step` folder)
 3. Small Step (`small-step` folder)
 
There is also a type inference algorithm (`type-inference` folder)

Each sub-project contains a demo (screenshot/gif) and instructions to run it.


## Demo
Online: http://stefann.eu/abstract-machine

<p align="center">

  <img src="https://github.com/stefan-niculae/abstract-machine/raw/master/abstract-machine-app/webapp/demo.gif" alt="Demo Gif"/>

</p>

Inspect the results of parsing your program and its configuration state after each step of execution.
Provides error messages and guards against infinite cycles and division by zero.


## Naming & History
The name of the repo is "abstract machine" as this is what the project initially contained. Afterwards, other execution models were added and the AM model was moved to its own folder, "abstract machine app".
