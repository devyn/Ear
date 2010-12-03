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

First, Ear loads. It may take a little while to load, as Alpha is expected to
be slow. You'll see messages like this

    Loading 'essential' environment... (5/24)

followed by the `Ready.` status in the previous example once Ear has finished
loading.

The `[e]` prompt shows that we are in `elimination` mode, that is, Ear will
attempt to find the equivalent form of what you entered which uses the least
patterns. for example, `(succ zero) * (succ zero)` involves 3 patterns
(`_ * _`, `succ _` and `zero`), where an equivalent form of it, `succ zero`,
only involves 2 patterns (`succ _` and `zero`).

So, back to the first entry.

    [e] succ zero
     =  succ zero

The form of `succ zero` with the least number of patterns is `succ zero`. That
is, Ear has found that `succ zero` already contains the least number of
patterns possible (at least, thus far).

    [e] (succ (succ zero)) * (succ (succ zero))
     =  succ (succ (succ (succ zero)))

This is the same thing discussed above. What we entered contains 7 entities in
3 patterns, and the result Ear found has 5 entities in 2 patterns. In the
`elimination` strategy, it is the number of patterns that count; that's how it
found the answer.
