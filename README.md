# Ear

Ear is a general-purpose programming language designed on the principles of
mathematical equivalence. An Ear program is simply a list of equivalent
patterns.

Here is an example of an Ear number implementation:

            num zero     = true
    num A | num (succ A) = true
            num X        = false

    zero + A     = A + zero
    A + zero     = A
    A + (succ B) = succ (A + B)

    zero * A     = A * zero
    A * zero     = zero
    A * (succ B) = A + (A * B)

We  will  be dissecting this example  throughout  this article.

## Elimination

    $ ear
    Ear Alpha-xxxxxxx
    Ready.
    [e] succ zero
     =  succ zero
    [e] (succ (succ zero)) * (succ (succ zero))
     =  succ (succ (succ (succ zero)))
    [e]
