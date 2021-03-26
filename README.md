# Zip II #

This library implements zippers as published in [Functional Pearl, The Zipper](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf) by Gérard Huet.  It also provides a namespace (`com.hapgood.zipii`) with a high degree of compatibility with the original Clojure implementation in [clojure.zip](https://github.com/clojure/clojure/blob/master/src/clj/clojure/zip.clj).

### Goals ###
In rough priority order, the goals of this implementation are as follows:

1. Implement a full-featured basic Zipper as described by Huet.
2. Implement an equally full-featured Scar Zipper using the same API as the basic Zipper.
3. Have no dependencies on external libraries.
3. Support serialization of the zipper (the Loc record and its basis components).
4. Use "modern" and idiomatic Clojure, specifically:
   1. Use data-laden exceptions where appropriate.
   2. Leverage protocols to allow for extensions.
   3. As in the original Clojure implementation, support user-selectable data structures as the branch (interior) nodes of the tree.
   4. Be host-agnostic.
5. Support a `down-to` function applicable to navigating within Clojure's indexed data collections (map-like and vector-like).
6. Provide a `clojure.zip` compatibility mode in the `com.hapgood.zipii` namespace.
7. Perform well updating trees.
8. Perform well for navigating trees.
6. Contain the Clojure-specific implementation details into as small a volume as possible (hopefully this improves the readability of the code if one uses Huet's publication as a starting point).

I've also included a test suite which has been immensely useful as I have iterated on the design.

### To-Do ###
Things I would like:

1. Better documentation -especially around the Zip protocol's differences from the original Clojure implementation which opts for three user-supplied functions for the same functionality.
2. More comprehensive test coverage.
3. Better understanding of the original xml-zip function ... is it really so open to defining a node that an integer is a branch?
4. A ClojureScript port.  This should not be hard as I have avoided any Java interop and there is no dependency on external libs.
4. A better implementation of the clojure.zip-compatible `remove` function -my implementation is adequate but not likely optimal.
5. A performance comparison between the protocol-based approach I use and the original clojure.zip approach.
6. Some feedback on the value of foregoing tracking changes (as compared to the original Clojure implementation).  Is the performance hit bad for real-world work?  Is there a clean way to have my cake (no `changed?` attribute) and eat it (high read-only performance) too?
