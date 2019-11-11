# SDP for Vector

SDP for Vector is an implementation of SDP classes for Vector.

## Reasons

Vector is a great library that focuses on efficiency and usability. However, it
seems to me that its developers underestimated their capabilities. If they had
written a more general library of arrays, not limited only to vector operations,
I would now be busy with something more useful.

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

## Versioning

sdp4vector follows of the [Haskell PVP](https://pvp.haskell.org) and SDP wrapper
rules.



