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

My solution is found in `index.erl`. We index the lines and then split each line into words which are then normalized, filtered, sorted and grouped.

### Taking it further

> Removing all short words (e.g. words of length less than 3) or all common words (you‘ll have to think about how to define these).

Valid words are checked with `valid/1`, which ensures that words are letters and apostrophe only and that they are at least 3 characters long I didn't add the logic to remove common words, but this is where I would put that check as well.

> Sorting the output so that the words occur in lexicographic order.

The algorithm used requires that the words are sorted and so this is already taken care of.

> Normalising the words so that capitalised ("Foo") and non capitalised versions ("foo") of a word are identified.

I wrote a `normalize/1` function to take care of this by converting words to all lower case.

> Normalising so that common endings, plurals etc. identified.

I didn't do this part, but that would also be part of the `normalize/1` function.

> (Harder) Thinking how you could make the data representation more efficient than the one you first chose. This might be efficient for lookup only, or for both creation and lookup.

The current implementation is *O*(*n* ln *n*) for creation and *O*(*n*) for lookups. I suppose using a binary search tree could reduce the lookup time to *O*(ln *n*)* or hash map to reduce the creation time to *O*(*n*) and lookups to *O*(*n*) with the draw back that we lose sort order. Both operations seemed fast enough on my machine with the Dickens text and so I didn't put the time in to trying either.