[//]: # (To preview markdown file in Emacs type C-c C-c p)

# Advanced Fortran Topics 

The repository contains code from the hands-on sessions within the PRACE online
course given by the Leibniz Supercomputing Centre of the Bavarian Academy of
Sciences and Humanities.

## Overview of the course

From the [PRACE web page](https://events.prace-ri.eu/event/1039/):

> This course is targeted at scientists who wish to extend their knowledge of Fortran to cover advanced features of the language.

> Topics covered include (still preliminary)

> - Best Practices
>    - global objects and interfaces
>    - abstract interfaces and the IMPORT statement
>    - object based programming
> - Object-Oriented Programming
>    - type extension, polymorphism and inheritance
>    - binding of procedures to types and objects
>    - generic type-bound procedures
>    - abstract types and deferred bindings
> - IEEE features and floating point exceptions
> - Interoperability with C
>    - mixed language programming patterns
> - Fortran 2003 I/O extensions

> - OO Design Patterns: application of object-oriented programming
>    - creation and destruction of objects
>    - polymorphic objects and function arguments
>    - interacting objects
>    - dependency inversion: submodules and plugins
> - Coarrays
>    - PGAS concepts and coarray basics
>    - dynamic entities
>    - advanced synchronization
>    - parallel programming patterns
>    - recent enhancements: collectives, events, teams, atomic subroutines
>    - performance aspects of coarray programming

## Prerequisites

> A recent version of the gfortran compiler (if possible, including the Opencoarrays package for parallel programming) should be installed.
> Alternatively, an evaluation version of the NAG compiler (version 7.0) can be used. The latter can be obtained from https://www.nag.com/content/software-trials?product=NAG%20Compiler 
 
> Finally, the GSL (GNU scientific library) and its header files should be installed.

The GCC Fortran front-end (gfortran) v. 5.1 and later employ OpenCoarrays
to support parallel execution, so having a new version of gfortran and GSL should cover it.
To this end, on the C3SE cluster Hebbe one can load the appropriate modules by executing
`source load_modules.txt` from the repository's top directory.
