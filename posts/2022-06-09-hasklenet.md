---
title: Haskle.net
author: Clément Delafargue
tags: haskell, elm
---

A few days ago, I thought "haha, haskle, that sounds like wordle, but for haskell".

Fast forward two days, and I’m releasing [haskle](https://haskle.net). It’s a small
wordle-like game, where the goal to guess a function from prelude given its (obfuscated)
type. The type elements are gradually revealed as you try names. It’s really fun for
functions with descriptive types (eg `🤷 🤷 => 🤷 🤷 => (🤷 ->🤷 🤷) -> 🤷 🤷 -> 🤷(🤷 🤷)`),
not so much when you’re in `Floating` land. Still, I’ve learned about a lot of numeric
functions that I had never used until now.

It’s [built with elm](https://github.com/divarvel/haskle/blob/main/src/Main.elm), as I
wanted something refreshing after having used purescript extensively at `$PREVIOUS_JOB`.
Elm is fun, albeit a bit frustrating at times when you’re used to haskell and purescript,
but it has be quite enjoyable overall.
