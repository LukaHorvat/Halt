# Halt
A predictable scripting language

### What
The [halting problem](http://en.wikipedia.org/wiki/Halting_problem) sucks. It means the only thing
you can do when running code in some sort of an environment is trust the person who wrote it or
time it's execution and then terminate when you think it's been running too long.

**Halt** helps by limiting the things you can write so that not only can it be determined if your
program will finish, it can also tell you the maximal time it could possibly take.

### Why
Have a game that you'd like to add modding support to? This might prove useful.

You could allow third-party code to hook into your update loops without worrying it will impact the
responsiveness of the game. You could allow slower code in less time critical parts.
And you can tell the modder his code is too slow *before* some runtime benchmarking tells him so.

### How

```
time 100
    acc = 0
    for i from 1 to 50
        acc += i
```

This code reports an error because the specified block takes longer than expected.

**Halt** compiles away the syntactical sugar to make calculations easier to do. In the above code,
only the `+=` gets compiled to `acc = acc + 1`.

There are two ticks in the body of the loop and a couple more for the loop itself so it sums up to more than 100 ticks.

Say increase the time limit to 500. Now we get a warning because the code takes less than specified.
That's the point of the `time` statement. It's there to help the programmer. You assume how long it takes, and then the compiler tells you when you go over the limit.

There are some scenarios where the maximum cannot be determined. In those cases, the time statement has a more important role.
It makes a guarantee to the compiler that the code block will not take longer than specified.
That guarantee is enforced at runtime, so if it ends up taking more time, an exception is thrown.

### Details
The compiler doesn't speak in seconds or CPU instructions. The code may be interpreted on a very fast or a very slow computer so we can't make any guarantees about the time something will take. What **Halt** does talk about is the number of ticks.
Then it's up to the programmer to determine what a value of a single tick is.

##### Restrictions
There are things you can't do with **Halt**. The limitations are pretty big so it's really something to be aware of.

You can't recurse. A function can't call itself or any other function defined after it.
That's the functional equivalent of looping/jumps, so of course, loops themselves are also
limited.
There's no `goto`. There are no `while` loops. You can use `for` loops with constant
bounds, or with dynamic bounds but with specified constant maximum of those bounds.

Something interesting is what's considered a constant maximum.
You could define a function like
```
int sum (const bound, [int] list)
    acc = 0
    for i from 0 to length list | bound
        acc += list[i]
    return acc
```

This function's loop is bounded, but the bound is parametric.
When you call the function, you are required to pass in the bound. This can either be a number literal, or another constant parameter.
