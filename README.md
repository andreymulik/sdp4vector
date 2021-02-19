# SDP for Vector

SDP for Vector is an implementation of SDP classes for Vector library.

## Reasons

`vector` is a great library focused on efficiency and usability. `sdp` and
`vector` perform the same task, but focus on different things:

* `vector` provides well-optimized `Int`-indexed arrays, encouraging simple and
effective solutions. `sdp` allows you to refer to elements of its structures by
offset, but encourages more general solutions.
* `vector` and `sdp` refer to language extensions differently. `vector` uses
several unusual (`ParallelListComp`,` ExistentialQuantification`,
`KindSignatures`) and questionable extensions (deprecated` Rank2Types` and
`ScopedTypeVariables`, which is easy to avoid in most cases). `sdp` mainly uses
type system extensions and non-standard syntactic constructs to deal with
primitive types and operations. However, `sdp` is simpler as it doesn't use
higher-rank polymorphism.
* Both `vector` and `sdp` can work with mutable and immutable data. `vector`
provides some practical but limited functions for this. `sdp` implements two
approaches: in the general case, this problem is solved using hidhly generalized
conversion functions (`fromIndexed`, etc.) which should work with a wide variety
of structures, but for specific cases `sdp` also have a lot of conversions
(`fromFoldable`, `listR`, `thaw`, `freeze`, etc.).
* `vector` concentrates on efficiently solving practical problems, while `sdp`
tries to provide the widest selection of operations, to be easy to entry, keep
good extensibility and modifiability of behavior (through intermediate
abstraction layers like `Index` and `Unboxed`).

The combination of `sdp` and `vector` is a combination of versatility and
convenience with efficiency and pragmatism.

## Functionality

The capabilities of this wrapper are limited by the subject area of the `vector`
library and my penchant for lazy programming:

* The library doesn't yet contain instances for `Set`.
* `primitive`-based garbage. To begin with, I hate the `primitive` package and
strive to destroy it. But seriously speaking, the difference between `Prim` and
`Unboxed` is only in the name, and `PrimMonad` is almost `MonadIO`. If I wanted
to, I could write an "adapter" for this outdated horror in one evening, but I'm
a lazy butt.
* `Storable` vectors. `sdp` can work with pointers, but for the most part avoids
them. Possibly, support for `Ptr`-based structures will be added after stable
release of `sdp-foreign`. Now this is just another not very useful vector.
* Mutable vectors because they just makes immutable operations strict. I don't
want to wrap pure evaluations into a monad without special need.

## Versioning

`sdp4vector` follows of the [Haskell PVP](https://pvp.haskell.org) and `sdp`
wrapper rules.

