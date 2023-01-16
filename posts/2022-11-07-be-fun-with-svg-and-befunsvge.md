---
title: Be fun with SVG and befunsvge
author: Clément Delafargue
tags: haskell, svg
---

Finding creative ways to express yourself is not always easy when you’re
all thumbs and you can’t keep the beat. Since I’m not entirely bad at
programming, I gave generative art a try. I still get rubbish results,
but I am having fun doing so. That’s success, as far as I am concerned.

I’ve been spamming my poor followers with ugly shapes and inscrutable
code for a few days, so I tought it would be a good time to explain
what I was doing.

tl;dr: I generate SVG through an esoteric stack-based language inspired
by [befunge](https://esolangs.org/wiki/Befunge). You can try it out
online: <https://befunsvge.cleverapps.io>, but you might want read
a bit more to be able to actually use it.

![A kind of heatmap drawn with circles](/files/befunsvge-map.png)

![The code that generated the output presented above](/files/befunsvge-map-code.png)

[A link to the same program on a live playground](https://befunsvge.cleverapps.io/?req=eyJjb25maWciOnsiaGVpZ2h0Ijo1MDAsInNlZWQiOjc0NTQ2NTI4NjczODQzMjY5MTcsIndpZHRoIjo1MDAsInNvdXJjZSI6eyJ0YWciOiJQZXJsaW4iLCJjb250ZW50cyI6eyJzY2FsZSI6OC4wZS0yLCJvY3RhdmVzIjo1LCJhbXAiOjgsInBlcnNpc3RlbmNlIjowLjF9fSwibWF4SXRlciI6MTAwMDAwMH0sInByb2dyYW0iOnsic2l6ZSI6WzU1LDVdLCJncmlkIjpbW1swLDBdLCJ2Il0sW1swLDFdLCIwIl0sW1swLDJdLCI-Il0sW1swLDNdLCJeIl0sW1sxLDJdLCI6Il0sW1sxLDNdLCIrIl0sW1syLDJdLCJcIiJdLFtbMiwzXSwiMSJdLFtbMywyXSwiZCJdLFtbNCwyXSwiXCIiXSxbWzUsMl0sIjIiXSxbWzYsMl0sIi8iXSxbWzcsMl0sImAiXSxbWzgsMl0sIiMiXSxbWzksMV0sIkAiXSxbWzksMl0sIl4iXSxbWzEwLDJdLCJfIl0sW1sxMSwyXSwiOiJdLFtbMTIsMl0sIjkiXSxbWzEzLDJdLCIwIl0sW1sxNCwyXSwicCJdLFtbMTUsMl0sIjAiXSxbWzE2LDJdLCI-Il0sW1sxNiw0XSwiXiJdLFtbMTcsMl0sIjoiXSxbWzE3LDRdLCIrIl0sW1sxOCwyXSwiXCIiXSxbWzE4LDRdLCIxIl0sW1sxOSwyXSwiZCJdLFtbMjAsMl0sIlwiIl0sW1syMSwyXSwiMiJdLFtbMjIsMl0sIi8iXSxbWzIzLDJdLCJgIl0sW1syNCwyXSwiIyJdLFtbMjQsM10sIiQiXSxbWzI1LDJdLCJ2Il0sW1syNSwzXSwiPCJdLFtbMjYsMl0sIl8iXSxbWzI3LDJdLCI6Il0sW1syOCwyXSwiOCJdLFtbMjksMl0sIjAiXSxbWzMwLDJdLCJwIl0sW1szMSwyXSwiOiJdLFtbMzIsMl0sIjUiXSxbWzMzLDJdLCIyIl0sW1szNCwyXSwiKiJdLFtbMzUsMl0sIioiXSxbWzM2LDJdLCI5Il0sW1szNywyXSwiMCJdLFtbMzgsMl0sImciXSxbWzM5LDJdLCI1Il0sW1s0MCwyXSwiMiJdLFtbNDEsMl0sIioiXSxbWzQyLDJdLCIqIl0sW1s0MywyXSwiOCJdLFtbNDQsMl0sIjAiXSxbWzQ1LDJdLCJnIl0sW1s0NiwyXSwiOSJdLFtbNDcsMl0sIjAiXSxbWzQ4LDJdLCJnIl0sW1s0OSwyXSwiMCJdLFtbNTAsMl0sImkiXSxbWzUxLDJdLCI1Il0sW1s1MiwyXSwiKyJdLFtbNTMsMl0sIs66Il0sW1s1NCwyXSwidiJdLFtbNTQsNF0sIjwiXV19fQ==)

## SVG generation for pen plotting

Pen plotters are tons of fun: instead of printing bitmaps, they move
an arm with a little pen over a piece of paper. Parametric curves
are a great fit for plotters, as they provide a super crisp output.
Programatically generating SVG shapes is also relatively easy, so
that’s an easy way to get started. Well I bought a shitty $100
plotter over ali express and never managed to get it to work, but I
had fun nonetheless. I spent a fair bit of time drawing tesselated
penrose triangles and I’m happy with the result, even though it’s
still on a screen and not on a piece of paper.

![Tesselated Penrose triangles](/files/penrose-random.svg)

After a bit, it became boring though, because while writing haskell
is something I tremendously enjoy, it is also something I’m
quite comfortable with now, so I lacked friction to guide my visual
experimentations (don’t worry, Haskell is very good for abstract
experimentations and provides me with ample head-scratching in this
area, but that’s something different).

## Befunge

Befunge is an esoteric language created in 1993. It is stack-based,
but the fun part is that the execution model is based on a 2d
grid: the source code itself. Each character in the grid is
a command, and the execution pointer moves in a direction,
executing commands in sequence.  Control flow is handled by
commands changing the pointer direction.  Some are unconditional,
like `<^v>` (they change the pointer direction), and some are
conditional, like `|` or `_` (they change the pointer direction,
_based on the contents of the stack_). Basic arithmetic is provided
(`+-*/`), comparisons with <code>&#96;</code>. The [whole list of
instructions](https://esolangs.org/wiki/Befunge#Instructions) fits
on a page, so it’s relatively easy to understand and memorize it.
Writing actual programs, on the other hand, that’s a bit more
challenging.

Here are a couple examples:

A hello world

```befunge
"!dlroW ,olleH">:v
               |,<
               @
```

A factorial

```
&>:1-:v v *_$.@ 
 ^    _$>\:^
```

The original befunge (`befunge-93`) was later expanded into a family
of languages ([`funge-98`](https://esolangs.org/wiki/Funge-98)),
which generalizes befunge over the number of dimensions, and adds
new features such as concurrency).

## Befunsvge

I played a bit with generative art first with Processing, then in
pure JS and haskell, but sometimes the lack of constraints is a big
blocker in itself.  So I wondered if it would be fun to generate SVG
from a purposefully constraining language, and befunge struck me as
a good candidate. I don’t need a lot of abstraction to draw SVG,
just math operations, and stack-based languages are _fun_.

So I wrote a befunge-93 implementation, enriched with a couple
generative-art-oriented extensions.  It’s available as a
[command-line tool](https://github.com/divarvel/befunsvge), as well
as an [online playground](https://befunsvge.cleverapps.io). You
can share links to the playground, program and configuration are
stored in the URL.

### A first example

```befunsvge
0 >:"d"`#v_:5*0M:"d"\-5*0\Lv
1+^      >π@               >
```

([Try it out online](http://befunsvge.cleverapps.io/?creq=H4sIAAAAAAAAA1WQwU7DMAyG38XcIJvirOm6HhBXJJC4dwOqLnSR1nZKw0BMlXhDXonE3lDI6ftix_mTEzRD_2ZbKE-wM7bdeSi1lAJGY7ZQzpTKiiJbyGKV5ZirDHWofdit3136hnfXmHjc12EKPBm3tz2IONeb3o-xNDb1PvTouTSzhYCh8fXRhIoWUHcHKFUYdDButGM4EqfJuZ4mAV39ee-NgxJlXGHn4IbW1R0NtV-hs1KFUBsBrbMhb1VVUsigEjYiMgZGYiS-IVbUc3vmuP9MvKD9kjgjXgOJJtkS52lhSfJKXBBfEa-Ij2fGv9uQ471cJFZ-vtkwuR057h0L59UsHPKaJUtei5zykSVPpy3TzFgkr8FVWlIcb71m40QzljSESkOoNITS_yZwigeWZfIpQc6_spmmX9oCfqSGAgAA))

The important part here is the loop:

```befunsvge
0 >:"d"`#v_   v
1+^      > @  >
```

``:"d"` `` compares the counter to `100` (`d`'s ascii code is
`100`). `_` conditionally jumps to the end of the loop when the
counter reaches `100` (`_` moves to the left, then `v` routes to
the line below, and `@` ends the program). Before that, `_` moves
to the right, to `v`, then to `>` then to `1+` (by wrapping around),
and then the flow goes back to the comparison bit. The jump `#`
command allows to jump over the `v` when coming from the left. This
makes the code a bit more compact.

Then, `:5*0M:"d"\-5*0\L` generates `M 0 5*i L 5*(100- i) 0`, with
`i` the current loop counter (remember, `:` duplicates the top of the
stack, and `\` swaps the first two elements). We need to do a bit of
stack magic to not forget the current `i` value, and be able to compute
`5*i` and `(100 - i) * 5`. `M` and `L` pop values from the stack and
generate the corresponding SVG path commands.

### SVG extensions

The goal of befunsvge is to generate files amenable to pen plotting, so
it only support drawing shapes, without support for color or filling.

Since it’s easy to type greek letters with my keyboard layout,
I’ve decided to use greek letters for SVG-level commands:

- `ρ` (_rho_) pops coordinates for a rectangle and outputs a
  `<rect>` tag;
- `ε` (_epsilon_) pops coordinates for an ellipse and outputs a
  `<ellipse>` tag;
- `π` (_pi_) flushes the current path and outputs a `<path>` tag.

#### Path commands

Path is the most versatile tool in befunsvge (and possibly in
SVG). It works with a `d` parameter that contains a description of the
path. It works with a curser that can move to specific (absolute or
relative) positions, and draw lines, arcs or Bézier curves between
points. Befunsvge has special support for it: a dedicated buffer
for a path description, and operators that push commands to the
buffer. Finally, `π` flushes the buffer by generating a `<path>`
tag with the buffer contents.

- `M` (resp. `m`) moves the cursor to an absolute (resp. relative)
  `x,y` position without drawing anything;
- `L` (resp. `l`) draws a line to the absolute (resp. relative)
  `x,y` position;
- `H` (resp. `h`) draws a horizontal line to the absolute
  (resp. relative) `x` position;
- `W` (resp. `w`) draws a vertical line to the absolute
  (resp. relative) `y` position (in SVG it’s `V` and `v`, but `v`
  is already part of befunge);
- `A` (resp. `a`) draws an ellipse arc to the provided absolute
  (resp. relative) `x,y` position, and the provided
  `rx,ry,angle,large-arc-flag,sweep-flag` parameters;
- `CcSsQqTt` draw various Bézier curves.

The [MDN post on
`d`](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d)
gives you more detail about each commands. Befunsvge pops the
parameters in the same order (but from the top of the stack, so `01M`
outputs `M 1 0`).

### Perlin noise

Generative art relies a lot on randomness, since it’s a good way
to generate unique output based on a set of otherwise deterministic
rules. The most common (I think) source of randomness is [Perlin
noise](https://en.wikipedia.org/wiki/Perlin_noise). This provides
randomness with nice locality characteristics (pure random noise is
not very insteresting after all). A common use is to prodecurally
generate terrain.

It might be possible to implement Perlin noise in befunge-93, but
that would be a bit of a hassle, so befunsvge directly implements
it as an _input function_ `i`: it pops `x,y,z` off the stack, and
pushes back the Perlin noise value for these coordinates. External
configuration allows configuring the Perlin parameters themselves
(octaves, persistence, amplification).

## What’s planned next

Playing with befunsvge has been a lot of fun, but I still have a lot
of ideas for further improvement.

### Bitmap input

One avenue I would like to explore is reading input from an external
source, typically images, so that values for individual pixels
can be used. The input function `i` was build with this in mind:
external configuration. Currently it only supports Perlin noise, but
it could be extended to other kinds of input. The issue is that it
would then be impossible to mix several input sources.

### Trig functions

Befunge-93 provides support for basic arithmetic (`+-*/%`), but there
is nothing available for trigonometric functions.  Such functions
are fundamental when drawing circular shapes. Befunsvge support for
SVG arcs and Bézier curves allows me to cheat a little, but that is
not enough. The issue here is that these functions operate on real
numbers, not on integers, while Befunge (and Befunsvge) only provide
integers. Extending befunsvge to support floating-point numbers is a
can of worms I would very much like _not_ opening. Another solution
would be to work on degrees and take an amplification parameter.

### Step debugger

Writing simple befunsvge programs can become easy after some time,
but the development experience is cleary painful. The web UI doesn't
display anything, and the command-line runner only displays the stack
contents after execution has stopped. A nice addition would be a step
debugger, allowing to step through program execution, inspecting the
stack, and maybe adding breakpoints. I am not sure how to do it yet,
and whether to have it embedded in the browser. Depending on what I
want to do, I might have to rewrite the whole engine.

## In conclusion

Was I able to use a pen plotter to draw nice things and put them
on my wall?  
No.  
But was I able to generate pieces that could be plotted and put
on a wall?  
Also no.

Anyway I have fun writing abtruse programs in an obscure language,
generating pictures that don’t really make sense. Sue me.
