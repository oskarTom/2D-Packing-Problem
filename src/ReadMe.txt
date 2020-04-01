

To compile, run the following commands:

$ gfortran -c mtfort90.f90 SAalgo.f90 PackingProblem.f90
$ gfortran mtfort90.o SAalgo.o PackingProblem.o -o run


To pack rectangles compactly, change < or > to <= and >= respectively in all IF statements inside the testR function.
