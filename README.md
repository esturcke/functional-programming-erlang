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

`bits:bits/1` is the regular recursive version. It adds 1 if the number is odd to a recursive call counting the number of bits of the number divided by 2.

`bits:bits2/1` uses the same algorithm but with an accumulator and tail recursive call.

On large numbers `bits:bits2/1` is faster. I suspect this is because it reuses the stack for the recursive call.

## Week 2 - Indexing a file

> The aim of this exercise is to index a text file, by line number. We can think of the input being a list of text strings, and below we’ve provided an outline Erlang module that reads text files into this format, as well as a couple of example files to process.
>
> The output of the main function should be a list of entries consisting of a word and a list of the ranges of lines on which it occurs.
> 
> For example, the entry
>
> `{ "foo" , [{3,5},{7,7},{11,13}] }`
>
> means that the word "foo" occurs on lines 3, 4, 5, 7, 11, 12 and 13 in the file.

My solution is found in `index.erl`. First, we number lines and then split each line into words which are then normalized, filtered, sorted and grouped.