---
title: Intro to functional programming
author: Clément Delafargue
tags: fp, teaching
---

I've been faced with an interesting challenge: teach Functional Programming
to last year students at [Centrale Nantes](http://www.ec-nantes.fr).

##Some context

The students did not have any background in FP, even though they have a strong
level in math after two years of preparatory classes.

Most of them have only seen C and Java.

I got a two hour window to make them discover FP.

##Choices

I've found it a bit difficult to design this course. Should I show them just a
functional language and let them infer functional programming from it ? Should
I talk about more abstract notions and stay closer to the essence of FP ?
I chose to stand in middle ground, and split the presentation in two parts:
the first part would be about fundamental notions about FP, the other part
would be the presentation of a functional language and a quick problem-solving
session with it.

##Contents

I began by describing different programming paradigms and insisted on the
declarative nature of FP. I then proceeded on explaining a few key
characteristics of FP (immutability, recursion, limited side effects).

I also chose to mention quickly the Curry-Howard isomorphism to show the deep
relation between proofs and programs. I did not have the time to elaborate on
type systems, though.

After having explained the fundamental parts, I went on with Haskell. Chosing
Haskell over other functional languages was easy, it's the purest yet
practical language I know of. I could also have used SML, but I have a poor
knowledge of it.

I began by running quickly over Haskell's syntax. I could have insisted a bit
more on pattern matching.

During the rest of the session, I solved small problems on the whiteboard to
show students the functional problem-solving mindset. A few of them were quite
active and got it quite fast.

The feedback I got from the students and a teacher who attended the class was
generally positive. It was a bit fast and technical, but interesting.

[Slides (in French), PDF](/files/intro_fp.pdf)

[Slides (in French), markdown](https://github.com/divarvel/gelol-exolang/blob/master/slides.md)

I should do it again next year and I'm looking forward feedback from
functional programmers. If you have suggestions, please let me know, the
current structure is far from being perfect and I'm willing to improve it.
