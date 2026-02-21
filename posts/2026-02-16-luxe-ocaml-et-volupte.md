---
title: Luxe, ocaml et volupté
author: Clément Delafargue
tags: ocaml
---

After a couple years using rust as my primary language, I’ve got a new job where I’m using a variety of languages (including rust and typescript), but mostly go [^1].

So I did the reasonable thing and decided to try using a new programming language. If you’re unlucky enough to be within hearing distance, you may have heard me ramble about why you should try ocaml. So I decided to follow my own advice. Also I spend too much time with [Xavier](https://xvw.lol).

## Why I like ocaml, _theoretically_

I’m vocal about why I like rust: great tooling, relatively modern features (it is expression-oriented, it has Algebraic Data Types, no `null`, no `nil`, relatively expressive types, great baseline performance). Note that I don’t think that its system-orientedness and memory management is key in all places where it’s used. I do think that it is used in many cases _despite_ its system-orientedness, just because the overall experience is so good it makes manual memory management tolerable. This brings the obvious question: what about a language that is both boasting nice tooling, a good type system, and a garbage collector, while being still lightweight and approachable (as much as I love haskell, and as much as I think claims about its steep learning curve are overblown, it still requires a strong commitment).

So, on paper, OCaml fits the bill for all this, plus a few nice bonuses like [unikernels](https://mirage.io/), [wasm generation](https://ocaml.org/tools/wasm-target), etc. That being said, ideas that seem great in my head don’t always end up being _actually great_.

It was clear that if I were to continue lecturing people on this, I would need to actually give it a try.

## What I set out to do

In a fun turn of events, I ended up doing the same thing I did in 2013 when I started using haskell thanks to hakyll: generate a static website.

OCaml has [YOCaml](https://yocaml.github.io/tutorial/index.html), a static site generator (actually a build system) heavily inspired from Hakyll (hakyll 3, not 4; which is interesting, but a discussion for another time).

I set out to rewrite [my homepage](https://github.com/divarvel/physical-homepage) (actually just a static list of talks, but served as a dynamic website) as a static website[^4].

This was a great fit since it allowed me to try out ocaml tooling in a real project, solving an actual problem within a confined scope (a single [^5] library, no need for ocaml-specific deployment).

I also tried [statichost.eu](https://statichost.eu) and it mostly worked. A few rough edges but it does what I want, no more, no less.

## First thoughts about ocaml

It’s a bit of a mixed bag for me. The language feels both modern and dated. Tooling is generally fast and nice, but a bit cumbersome and in an in-between. There are a lot of syntactic choices I like, but I still find the code a bit _too_ verbose.

### Tooling

I often joke that when using haskell, I love the language, but long for rust’s tooling, and when using rust, I love the tooling, but long for haskell’s features. Tooling matters a lot to me.

#### Opam

Opam gave me a weird flashback of the haskell situation in the early 2010s. Opt-in per-project isolation. Opam switches feel clunky. I like that the compiler is part of the dependencies. I don’t like that creating a new switch requires rebuilding the compiler (I’ve been told that this was being fixed, by making the compiler relocatable and as such shareable across projects).

In any case this feels extremely outdated compared to the rust ecosystem and even cabal v2. Managing environments by sourcing env vars instead of having tooling manage it directly is not-so-good.

A surprising thing (but I suspect it’s because of nixos’ packaging) is the system dependency on `bubblewrap` (in addition to the usual `gmp` and `pkg-config`).

#### Dune

Dune itself is nice. I could definitely see it take over the whole CLI (eg by calling opam under the hood). Having to call `dune build` to generate opam files in addition to actually build stuff was a bit weird. I think there is work going in the right direction for this, and I hope this will be fixed in the future, much like in haskell where the situation has greatly improved.

I’m not entirely sure how I feel about dune config files being s-exprs. At least it’s not yaml, sure.

A nice surprise was `dune build --watch` which provides a ghcid-like experience out of the box.

There are a few things that I found weird with dune, for instance `dune exec` requiring a `.exe` to target a module, and passing `@ocaml-index` to `dune build`.

I’m not sure whether it’s dune’s fault, but dune erroring on warnings is extremely annoying, especially for unused variables [^6].

#### `ocaml-lsp`

Honestly, for now it mostly just worked. It’s fast, it was easy to setup and install, and core features work (type lookup, go to definition, renames). I have tested a couple code actions, not everything made sense to me at the time.

Maybe it’s just that I worked on a small project, but it felt really fast.

The only thing that did not go well is cross module stuff (type checking, renames, etc). Everything works well within a module, but stuff broke easily across modules.

### Ergonomics

I’m really interested in ergonomics, because I’m now very used to ML languages, so I’m not wow-ed by basic features any more.

#### We get it, you have great type inference

That’s been my main gripe, especially as a beginner: the focus on type inference makes using top-level signatures un-idiomatic. In haskell you can omit them, but it’s considered bad style and `-Wall` complains about it.

In OCaml, you can use `.mli` files to define module signatures, which partially solves the issue (there is friction when jumping between files, and it only works for exported bindings), or add annotations directly within definitions. This is a bit cumbersome, but it works.

The reason why it frustrates me so much is that I start by thinking about types, and then fill out the implementation. Infering a type from an implementation feels absolutely backwards to me (of course I still value type inference within named definitions). I also get why separate signature files make sense for efficient separate compilation, but I feels like a technical optimization made at the expense of ergonomics. If I wanted to make my life harder just to make a compiler easier to write, I’d just write go [^1].

#### Modules

I don’t have enough experience with modules to rave about their merits compared to type classes, so I’ll focus on smaller ergonomics considerations.

Using local imports grew on me with rust. That makes unqualified imports way more palatable. I especially like how `open` meshes with `let` bindings. The local opens were a great discovery. I still need to see how they fare in practice, but it seems to me that it opens great eDSL opportunities.

#### Labeled and optional arguments

Haskell and rust don’t have optional arguments, so I kinda learned to live without it. In haskell, partial application makes it relatively painless (as long as you don’t have too many or there is a well defined precedence). In rust you can work around it with defining multiple functions or using structs with `Default` impls. It’s still cumbersome.

I think I like this feature, even though its use in the `Option` module made things extremely verbose compared to haskell.

#### `let` bindings

I was a bit surprised by `let … and … in`. In haskell there a single `let … in` for multiple bindings. In dhall, there is one `let … in` per binding (this also works in OCaml). In isolation, I don’t see the value in using `let … and … in` compared to multiple `let … in` bindings. I favor the latter (out of style, this feels more minimal, and out of practicality: repeated let bindings can be reordered easily). In haskell, there is redundancy with `where` blocks. I stopped using them altogether to favor `let in`, since it maps directly with expressions.

OCaml does not have `do` blocks, but does have `let*` for monadic bindings, and `let+` / `and+` for applicative bindings. For library authors, it’s a bit cumbersome because it has to be redefined in each module. For me, it was quite nice. I like the expressivity, compared for instance to rust’s `?`. In rust you can use multiple `?`s in a single expression, which can be useful from time to time, but can also make things hard to read. Compared to haskell, I like how it aligns more nicely with `let` bindings, and is less ambiguous than `ApplicativeDo`.

## Conclusion

Honestly, it’s nice. It’s nowhere near as terse as haskell, but it provides solid foundations, and the tooling is honestly promising, assuming that the `opam switch` weirdness can be hidden. I will need to spend more time with it, but I really liked the overall experience. Now I’m curious about the wider ecosystem, especially backend web development. I also want to try deploying unikernels.

[^1]: I don’t like go
[^2]: todo mirage os
[^3]: wasm of ocaml
[^4]: _if you like static websites so much, why did you build a dynamic website to serve static data?_ Well that’s a great question, dear reader I made up in my mind to serve as a rhetorical device. I wrote it at a time where I was working at a PaaS company that made serving haskell applications extremely easy, and at the time even easier than static websites. Now it’s easier to deploy static websites with TLS support, and I try to be frugal.
[^5]: more accurately a collection of cohesive libraries, but who cares?
[^6]: what is this, go? See `1.`
