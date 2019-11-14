# SDP for Vector

SDP for Vector is an implementation of SDP classes for Vector library.

## Reasons

Vector is a great library focused on efficiency and usability. I even think that
its developers underestimated their capabilities. If they wrote a more general
library of arrays, not limited to only vector operations, I would now do
something more useful.

* SDP provides arrays of a more general form - with their own interfaces for
indexes and unpacked elements.
* SDP relies on time-tested designs and carefully uses extensions.
* SDP provides capabilities that go beyond vector operations (Set, Map, Sort).

* Vector provides well-optimized arrays with the most efficient and frequently
used index type.
* Vector is much more active in using advanced language extensions both to
increase efficiency (type families, GADTs, high-level kinds), and to simplify
the code (ScopedTypeVariables).
* Vector offers many useful specialized operations.

## Functionality

The capabilities of this wrapper are limited by the subject area of the Vector
library and my own laziness:

* The library doesn't contain instances for Set, Map and other non-vector
operations. The only exception was made for Sort.
* Primitive vectors. SDP pseudo-primitives are more memory-efficient of
primitive wrappers from Primitive.
* Mutable arrays because they use PrimState. In addition, classes for mutable
structures are too general for MVector, but I don't want to once again wrap
clean calculations into a monad without special need.

## Versioning

sdp4vector follows of the [Haskell PVP](https://pvp.haskell.org) and SDP wrapper
rules.


