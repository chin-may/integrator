integrator
==========

This is a symbolic integrator.

Input should be given in prefix form and the variable wrt which integration is to be done should be provided.
eg.
(integrate '(+ 1 x) 'x)
(integrate '(log (+ x 3)) 'x)

It can handle polynomials, sin, cos, log, exponential and many combinations.
It can handle some inputs of the form f(g(x)).

Output is not simplified.
If it is not able to perform the integration, output will be of the form (INTEGRATE expr)
