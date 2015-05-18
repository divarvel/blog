---
title: FP patterns for scala beginners
author: Clement Delafargue
tags: fp, scala
---

I've given a talk about patterns common in FP, but not always well described
in scala:

 - Algebraic Data Types for data modelling
 - Typeclasses for extensible abstractions
 - Error handling in a composable fashion with Either and Validation
 - Segregation between logic and effects for easier testing
 - Property based testing

These patterns are not inherently functional but are the base on which
functional libraries are built in scala.

They're well described in Haskell, but their encoding in scala is not always
easy to find.

Here are the slides:

<iframe width="800" height="540" src="/files/embedder.html#http://clementd-files.cellar-c1.clvrcld.net/blog/fp-patterns-geecon.html" allowfullscreen />

