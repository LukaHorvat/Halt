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

Say we increase the time limit to 500. Now we get a warning because the code takes less than specified.
That's the point of the `time` statement. It's there to help the programmer. You assume how long something takes, and then the compiler tells you when you go over the limit.

### Details
The compiler doesn't speak in seconds or CPU instructions. The code may be interpreted on a very fast or a very slow computer so we can't make any guarantees about the time something will take. What **Halt** does talk about is the number of ticks.
Then it's up to the programmer to determine what a value of a single tick is.

##### Restrictions
There are things you can't do with **Halt**. The limitations are pretty big so it's really something to be aware of.

Loops are limited.
There's no `goto`. There are no `while` loops. You can use `for` loops with constant
bounds, or with dynamic bounds but with specified constant maximum of those bounds.

Something interesting is what's considered a constant maximum.
You could define a function like
```
sum :: Bound -> [Int] -> Int
sum bound list ->
    var acc = 0
    var len = List.length bound list
    for i from 0 to len | bound
        acc += list[i]
    return acc
```

This function's loop is bounded, but the bound is parametric.
When you call the function, you are required to pass in the bound. This can either be a number literal, or another constant parameter.

##### Ticks
You can think of ticks as something in between the big-O and the actual time.
Adding two integers is one tick. Comparing them, one tick. Calling a function, one tick.

Due to the language compiling to other languages, their respective compilers will most likely optimize
the code. **Halt** doesn't assume any optimizations. It talks about upper bounds and tries not to
make any assumptions about the lower ones.

##### Functions
**Halt** implements some functional programming aspects. All functions are curried and first-class.
There are lambda expressions.

Recursion has some important limitations.
Basically, a function cannot call itself, though that isn't strictly true.
No single function can reference another one that's defined after it, and it also cannot reference
itself. However, due to higher order functions, it is possible for a it to call itself via
the function passed to it.

For example, code like this works
```
f :: Int -> Int
f x -> x + 1

g :: Bound -> [Int] -> [Int]
g b xs -> map b f xs

map 2 (g 3) [[1, 2, 3], [4, 5, 6]]
```
even though `map` ends up calling itself.

##### Type system
Simple, with support for generic functions. Subtyping is not implemented.
Type inference is local. Variables can be defined with `var` if their type can be determined
from the right hand side or the assignment. It also works for lambdas.

Here's in an example of a `length` function.
```
length :: Bound -> [a] -> Int
length bound list ->
    var l = list
    var acc = 0
    for i from 0 to bound
        case l of
            x : xs
                l = xs
            []
                break
        acc += 1
    error "Length out of bounds"
```

##### Data structures
Algebraic data types with support for recursive definitions. However, it should be noted that,
while it's allowed to write a type like this,
```
data Stream a = Cons a (Stream a)
```
due to not being able to recurse infinitely, you actually cannot make an instance of it.
Type recursion in a negative position is also not allowed. You can't write
```
data Rec a = Constructor (Rec a -> a)
```

Full support for pattern matching on constructors is implemented.
Algebraic types are immutable.

Record types are also supported. They are collections of named fields.
```
record Character =
    name :: String
    health :: Int

Character cat =
    name = "Puss"
    health = 100

IO.print cat.name
cat.name = "Bob"
```
Records are mutable.
