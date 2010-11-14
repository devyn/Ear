# Ear

## What's this 'Ear'?

Ear is a declarative programming language with strong type inference and structural equivalence. It looks like this:

    []        = []
    A : (l B) = A : B

    l []      = []
    l (A : B) = A : B

    (A : B) + (l C) = A : (B + C)

    foldr Fn I []      = I
    foldr Fn I (X : L) = Fn X (foldr Fn I L)

    foldl Fn I []      = I
    foldl Fn I (X : L) = foldl Fn (Fn I X) L

## Writing an Ear program

An Ear program is really just a space of rules for equivalence.
Take this Peano-style unary natural number implementation, for example.

    zero       = zero
    succ (n A) = succ A
    n zero     = zero
    n (succ A) = succ A

This declares the following:

- There is `zero`, and it is equivalent to itself.
- There is `succ (n A)`, and it is equivalent to `succ A`.
  - This is the same thing as `n A == A | succ A = succ A` using guards, but more on that later.
- There is `n zero`, and it is equivalent to `zero`.
- There is `n (succ A)`, and it is equivalent to `succ A`.

You might wonder, "If `n zero` and `n (succ A)` are equivalent to their `n`-less counterparts, then why bother?"
The reason for this is actually quite simple. The pattern that `n _` uses is somewhat similar to a typeclass
in other languages like Haskell. `A` is a variable that can be anything. But `n A` is an `A` part of `n _`,
which means that `A` itself can now only be a `zero` or a `succ _`. This is used in the very definition of `succ _`:

    succ (n A) = succ A

See? If we had declared `succ A = succ A`, `succ _` could wrap anything! But of course we don't want that.
We only want it to be able to wrap numbers. So we restrict it to `n A`, which can only be `n zero` or `n (succ _)`.

Let's define the rules for addition of our numbers:

    zero     + zero     = zero
    (succ A) + zero     = succ A
    zero     + (succ A) = succ A
    (succ A) + (succ B) = succ (succ (A + B))

What exactly can we conclude from `_ + _`?

- `A + B = C`
  - `A = C` if `B` is `zero`
  - `B = C` if `A` is `zero`
  - `C` is `zero` if `A` and `B` are `zero`.
  - `n A`, `n B`, `n C`
  - `C >= A` and `C >= B`

That's quite a bit. This really comes in handy for error checking.

### Guards

Maybe you want to define a class of non-zero numbers.

    nonZero (n A) = A

Alright, but how do we ensure that `nonZero _` cannot match `nonZero zero`? Guards are the answer:

    A /= zero | nonZero (n A) = A

A guard is a boolean expression that makes specifications for a variable that the variable must fit.
Here, `A /= zero` must be `true`. This ensures that `A` is not `zero`, which is what we want.

Really, a guard is anything that satisfies `b A` where `A` can be `true` or `false`.

On a related note, if you define `x (y A)` and `x A`, and both match `A`,
whichever is more specific to `A` (in this case, `x (y A)`) will be used.

### Anonymous Structures

Somewhat like anonymous functions, Ear has *anonymous structures*. An anonymous structure is a set
of rules of equivalence describing a structure, `\`. Example:

    >> { \ A B = A * B } 2 3
    => 6

The structure must be a prefix structure, where `\` must only appear once.
For example, the following is not allowed:

    >> { A \ B \ C = A * B * C }
    !! syntax error: anonymous structure must be a linear prefix structure

### Partial Structures

Ear has its own style of partial application with *partial structures*. Watch them in action:

    >> addTwo = 2+_
    >> addTwo 2
    => 4

`2 + _` creates an anonymous structure of `{ \ B = 2 + B }`.

### Infinite Patterns

What if you wanted to define the standard list syntax, `[1, 2, 3, ...]`?
You'd need some way of expressing the `...`, and that's what infinite pattern notation is for.

    L([ _, _ ])~ = L(_ : _ : [])~

This here defines `([_,_])~` which will match `[1] = 1:[]`, `[1,2] = 1:2:[]`, etc.

`A(pre.. _ in.. _ post..)~` is a special pattern template variable which can
automatically reformat an infinite pattern from the LHS to the RHS of the equation.

#### Infinite Patterns and Partial Application

If you would like to partially apply an infinite pattern, you must do this:

    x = [(_,_)~]
    x = { \ A(_ _)~ = A([_,_])~ }

## Reduction

An Ear evaluator performs *reduction*. Its goal is to reduce everything
until you can no longer reduce anything. Consider this program:

    foldr Fn I [] = I
    foldr Fn I (X : L) = Fn X (foldr Fn I L)

    main = foldr (_+_) 0 [1,2,3,4,5]

Ear reduces:

    main = 1 + (foldr (_+_) 0 [2,3,4,5])
    main = 1 + 2 + (foldr (_+_) 0 [3,4,5])
    main = 1 + 2 + 3 + (foldr (_+_) 0 [4,5])
    main = 1 + 2 + 3 + 4 + (foldr (_+_) 0 [5])
    main = 1 + 2 + 3 + 4 + 5 + (foldr (_+_) 0 [])
    main = 1 + 2 + 3 + 4 + 5 + 0
    main = 3 + 3 + 4 + 5 + 0
    main = 6 + 4 + 5 + 0
    main = 10 + 5 + 0
    main = 15 + 0
    main = 15

Done.

## Validation

Ear can validate a program without actually reducing it. Consider the program from the last section.

- Is `main` valid?
  - Is `foldr (_+_) 0 [1,2,3,4,5]` valid?
    - We know that `(n A) + (n B) = n C` where `n _` is a class representing numbers.
      - `n _` for X and I throughout?
        - `n I == I` for all? Yes.
        - `n X == X` for all? Yes.
- `main` is valid.

If, however, we were to say `main = foldr (_+_) 0 [1,2,3,4,5,true]`:

- Is `main` valid?
  - Is `foldr (_+_) 0 [1,2,3,4,5,true]` valid?
    - We know that `(n A) + (n B) = n C` where `n _` is a class representing numbers.
      - `n _` for X and I throughout?
        - `n I == I` for all? Yes.
        - `n X == X` for all?
          - `n 1 == 1`? Yes.
          - `n 2 == 2`? Yes.
          - `n 3 == 3`? Yes.
          - `n 4 == 4`? Yes.
          - `n 5 == 5`? Yes.
          - `n true == true`? No. **STOP!**
- `main` is not valid because `n true = true` is not declared.

Ear will tell you that your program is invalid before you even go to run it.
