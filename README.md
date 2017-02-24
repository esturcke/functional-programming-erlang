# Functional Programming in Erlang (University of Kent)

Assignments for the [Functional Programming in Erlang MOOC](https://www.futurelearn.com/courses/functional-programming-erlang).

## Week 1

> Shapes
> Define a function `perimeter/1` which takes a shape and returns the perimeter of the shape.
> Choose a suitable representation of triangles, and augment `area/1` and `perimeter/1` to handle this case too.
> Define a function `enclose/1` that takes a shape an returns the smallest enclosing rectangle of the shape.

I used tuples to define the shapes for:

  - Squares `{square, {A}}`
  - Rectangles `{rectangle, {A, B}}`
  - Triangles `{triangle, {A, B, C}}`
  
where `A`, `B` and `C` are lengths of the sides.

> Define a function `bits/1` that takes a positive integer N and returns the sum of the bits in the binary representation. For example bits(7) is 3 and bits(8) is 1.
> See whether you can make both a direct recursive and a tail recursive definition.
> Which do you think is better? Why?

`bits:bits/1` is the regular recursive version. It adds 1 if the number is odd to a recusive call counting the number of bits of the number divided by 2.

`bits:bits2/1` uses the same algorithm but with an accumilator and tail recusive call.

On large numbers `bits:bits2/1` is faster. I suspect this is because it reuses the stack for the recursive call.
