---
title: Passing Multiple RTS options to a haskell executable
author: Clement Delafargue
tags: haskell, config
---

This post is just there to store a nugget of haskell configuration somewhere
and maybe help others. So if you're having trouble with `-with-rtsopts` and
multiple RTS options, here's how to do it:

```cabal
executable my-exe
  ghc-options:         -threaded -rtsopts "-with-rtsopts=-N -T"
```

Notice the quotes around the whole `-with-rtsopts` (you can also use multiple
`-with-rtsopts` flags). Turns out cabal splits on spaces and inserts quotes
everywhere. Yay shotgun parsers!

Now that's out of the way, here's a little context.

Thanks [\@alpmestan](https://twitter.com/alpmestan) for giving me the solution!

## Haskell RTS

> To make an executable program, the GHC system compiles your code and then links
> it with a *non-trivial* runtime system (RTS)

(The emphasis on *non-trivial* is mine).

The RTS handles garbage collection, light threading and a lot of other things.

You can pass `RTS` options to an existing binary:

	./my-exe +RTS -N -T -RTS
	GHCRTS="-T -N" ./my-exe

If you want to pass default options when compiling the binary, you can use
`-with-rtsopts` in your cabal config file.

The `-N` option enables the parallel runtime, which will use the multiple cores
to improve performance. `-N` is added by default in the `stack-new` template,
so you may already use it.

### GC Tracing

At [Clever Cloud](https://clever-cloud.com), we're working on a new metrics
system, capable of gathering user-defined metrics about applications. Since
haskell has ekg, we're working on a top-notch ekg integration :-)

I already gather WAI metrics (latency distribution, request count, error count,
etc…), but I wanted to also fetch GC stats. It's super easy to do with ekg
(`registerGcMetrics`), but it depends on runtime tracing to be enabled with
`-T`.  The ekg docs says `-T` is super lightweight and is a good default. I've
not done proper benchmarking about it though.

If you want an example of how easy it is to setup GC + WAI metrics with EKG,
and report it through statsd, have a look at my homepage:
<https://github.com/divarvel/physical-homepage/blob/master/app/Main.hs#L22>

## Keywords galore

Since I've had trouble finding a solution with a search engine, I'll just put
various queries I've tried in hope it will help other people find the solution.

	-with-rtsopts=-NT
        -with-rtsopts=-N -T
	-with-rtsopts=-N-T
	-with-rtsopts=-N-T
	ghc: unrecognised flag: -T
	ghc: unrecognised flag: -N

## Documentation

- <https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/runtime_control.html>
- <https://hackage.haskell.org/package/ekg-core-0.1.1.1/docs/System-Metrics.html#v:registerGcMetrics>
- <https://hackage.haskell.org/package/wai-middleware-metrics>
- <http://blog.clement.delafargue.name/posts/2017-05-04-listing-my-talks-and-slides.html>
