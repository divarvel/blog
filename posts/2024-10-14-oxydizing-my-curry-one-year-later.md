---
title: "Oxydizing my curry, one year later"
author: Clément Delafargue
tags: haskell, rust
---

In a somewhat *basic* move, I quit a Haskell job for a Rust job a bit more than a year ago. So it’s time for a small retrospective! Yay!

Note that I have not changed job because I wanted to stop using Haskell professionally, but because I had the opportunity to work on biscuit.

<!-- more -->

## The state of Haskell jobs

I have been both in the position of recruiting Haskell developers and looking for a Haskell job in the past few years, and things are in a weird state. In general I have noted an imbalance in favor of companies: lots of talented developers want to use Haskell at work. However, once you have some professionnal experience with Haskell it gets a bit better. But yeah, the pools for both canditates and opportunities are small, so timing is everything. Also, be prepared for working / hiring in a different country. Being comfortable with async / remote work is super important.

Actively looking for a job is always rough, anyways and it’s sometimes hard to not take things personally.

As for Rust, I think it has similarities with Haskell, but both the jobs/candidates pools are bigger, so things tend to be a bit more stable. Same as for Haskell, having professionnal experience with Rust makes things way easier. Lots of people want to start using Rust at work, not so many have the experience to help create and grow a Rust team.

The move from Rust to Haskell seems to be a common one, and I get it. Rust is way less niche than Haskell and somehow less scary to employers (I’ve always thought it’s a wrong impression, as I think Haskell has a smoother learning curve than Rust).

For me, I had the opportunity to work on [biscuit](https://biscuitsec.org) at work with a great team, so moving from Haskell to Rust was more of a tradeoff than a deliberate choice. My initial plan was to keep using Haskell at work.

## What does a year of Rust do to a diehard Haskell programmer?

I have been interested in Rust for a long time, Geoffroy Couprie and I first gave [a talk about it](https://blog.clement.delafargue.name/posts/2014-05-05-rust-talk-at-devoxxfr.html) more than _ten years_ ago, before `1.0` was even released.

I have used Rust at work since then (not as my primary language though) and was the first one to put Rust in production at Clever Cloud, which now uses it extensively.

This familiarity with Rust definitively helped switching to Rust as my primary language.

### What I miss from Haskell

I miss the high-level features of Haskell. I miss its garbage collector, I miss the [typeclassopedia](https://wiki.haskell.org/Typeclassopedia), I miss all the common typeclasses, I miss [`traverse`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#v:traverse), I miss [hoogle](https://hoogle.net), I miss [`Validation`](https://hackage.haskell.org/package/validation-selective). I miss [servant](https://hackage.haskell.org/package/servant). I miss [GeneralizedNewtypeDeriving](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/newtype_deriving.html). I miss [symbols](https://hackage.haskell.org/package/base-4.20.0.1/docs/GHC-Base.html#t:Symbol).
 And do I miss [`Applicative`](https://hackage.haskell.org/package/base-4.20.0.1/docs/Prelude.html#t:Applicative).

Rust is a great language, but it has been designed as a systems programming language, and it cares about things I don’t really need to care about in my day-to-day work.

Still, it has solid bases, traits and algebraic data types enable the same kinds of code modularity than in Haskell, immutability by default and constrained mutability provide a subset of guarantees enforced by Haskell’s purity.

Async programming is still cumbersome, even though the language continues to improve on that front. 

Everytime I use Rust, I miss Haskell’s features.

### What i do not miss from Haskell

I do not miss Haskell’s fragile tooling. Cross-compiling to static binaries is a breeze. I don’t have to debug the documentation generator every time I want to cut a release.

Everytime I use Haskell, I miss Rust’s tooling.

### Do i still use Haskell?

I still use Haskell outside of work. I maintain [biscuit-haskell](https://hackage.haskell.org/package/biscuit-haskell). I recently landed a PR in [servant](https://hackage.haskell.org/package/servant).

It’s not really Haskell, but I snuck in Elm in an internal project at work. The more I use elm, the more I see it as being a great language for learning and teaching, but not so good at anything else. Still, it allowed me to scratch my Haskell itch.

## Closing words

Both Haskell and Rust let me achieve a level of quality I deem necessary thanks to their focus on correctness. I am still more comfortable with Haskell because it lets me think at the level of abstraction I like. Rust forces me to think at a slightly lower level and that sometimes makes things a bit inconvenient. Still, its strong focus on correctness and solid design makes it more of an inconvenience than a real blocker.

The Rust ecosystem is way more dynamic and its tooling is more stable, so the frustration I sometimes have with the language is offset by the frustration I no longer have with the tooling.

I still think Rust’s adoption is not aligned with the level of abstraction it promotes, and that most programs would benefit from higher-level languages, but most of the high-level languages are so bad that Rust is still comparatively great as a high-level programming language, just because it’s such a good language.
Algebraic Data Types matter more to me than a garbage collector. Parametric Polymorphism matters more to me than having a unified string type. The absence of `null` matters more to me than automatic boxing.  

On a more pragmatic note, I think focusing on Rust is also a good career move, even though it meant I left a bit of Haskell expertise behind, a sizeable part of it transferred neatly to Rust.
