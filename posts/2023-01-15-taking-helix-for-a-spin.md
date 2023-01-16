---
title: Taking helix for a spin
author: Clément Delafargue
tags: text, tui, editor
---

I started using vim full-time in 2009. Back then full-featured editors like
emacs were tempting, but once hooked on modal editing, it was too late to
switch.

When neovim came out, it wasn’t packaged for
[exherbo](https://www.exherbo.org/) (the distro I was using at this time), so I
never got to really play with it, even though it was a massive improvement.

At some point around 2017 or 2018, I tried [kakoune](https://kakoune.org/).
Like vim, kakoune is a modal editor. Unlike vim, it strives to provide
feedback. The core tenet of kakoune’s model is to _provide a selection
before acting on it_ (contrary to vim, where you provide the action, then the
selection). So instead of typing `dw` to delete a word, you type `wd` to select
a word, and then delete it. This seemingly simple change unlocks a powerful
property: the selection is highlighted as you define it, so _if you want_,
you can check it’s correct or perhaps amend it before acting on it. Of course
for small actions like `wd`, you usually type it directly without thinking,
but when combined with incremental search and multi cursors, it becomes a
_more powerful_ and _less burdening_ way to use a modal editor. You still get
all the benefits like doing a lot of things without thinking or twisting your
fingers with 10-fingers chords, but you always have the option to get in a more
conscious feedback loop when you want.

Anyway long story short, I’ve used kakoune for 4 years or so, I’m sold on
its editing model. By choice, it does not provide a fully featured scripting
language like vim, so I ended up hacking a couple features on top of it with
[rofi](https://github.com/davatorium/rofi), installed `kak-lsp` to get `LSP`
support, and run a patched kakoune to display trailing whitespace. It’s good,
but rofi does not work over SSH (I sometimes SSH over to my main rig from my
aging laptop), plugins are fragile and my patch never got merged upstream.

And then a couple months ago, I hear about helix, dubbed a _post-modern
editor_. I’m super happy to see it’s inspired by kakoune, because I think the
kakoune model is superior. I give it a try from time to time. The built- in
fuzzy finder solves my main pain point with kakoune, and the rest is close
enough to kakoune for me to not be too lost. I find it interesting and tell
myself I should try it more seriously at some point. One remarkable thing: at
this point, I have not tweaked its configuration at all. It just works okay out
of the box.

And then, a week ago, I decide to try it more seriously, starting by reading
the doc and seeing how I can make it better for me.

## Basic configuration

The first thing I did was to display whitespace (yeah, I’m this kind of guy),
enable auto-format where I could, display rulers at 80 and 120 chars to
gently nudge me away from writing long lines.

Then, making the cursor a bar in insert mode was a nice little improvement I
found over the good old block. I try not to spend too much time in insert mode
anyway.

Then, making the file picker display hidden files because I edit them just as
much as normal files.

And finally, biting the bullet on having `space` as the leader key (on kakoune,
I had `,` which is right next to my left pointer finger on a bépo layout) to
get quick access to `:write`, `:quit` and "go back to the last buffer".

That’s it, the config file is just shy of 15 lines and I feel at home.

With that, I was able to make `helix` my main driver, about a week ago. It
took a few tweaks over the course of two days. It has been my fastest
transition period for such a core tool. Switching to vim and then kakoune
full-time took me weeks.

```toml
# the default theme is nice, but is not contrasted enough for me
theme = "zenburn"

[editor]
rulers = [80,120]
auto-format = true
# no support yet for `trailing`, but there is an open PR
whitespace.render = "all"

[editor.cursor-shape]
insert = "bar"

[editor.file-picker]
hidden = false

[keys.normal.space]
"e" = ":write"
"q" = ":quit"
"space" = "goto_last_accessed_file"
```

## A bit more config

With the basics set, it was time to look at bit more at other things that make
helix helix.

### Out-of-the-box LSP experience

Rust LSP features just work, as soon as you install `rust-analyzer` (`helix`
helpfully tells you that when you run `hx --health rust`).

So in any rust project, I can do all the cool LSP stuff:

- diagnostics are displayed inline
- list symbols (`<space>s`)
- rename symbol (`<space>r`)
- go to definition (`gd`)
- apply code action (`<space>a`)

Same for haskell, as long as you have `haskell-language-server-wrapper` in
`$PATH`.

### Tree-sitter grammars

Another strength of `helix` is native integration of tree-sitter grammars.
Instead on relying on a pile of regexes for syntax highlighting and custom
plugins for syntax-aware features, `helix` relies on actual grammars. Tree-
sitter provides support for incremental parsing, so parsing is super fast
during editing.

Once the document is parsed, the AST is used for coloring, but also for
movements (eg. `Alt-o` selects the parent node in the tree, `mif` selects the
current function body). Manipulating the syntax tree directly was something
that always impressed me when watching experienced lispers write code. Now I
can have that in any language.

Another benefit is that the cost of adding support for a language is amortized.
The hard part is writing the tree-sitter grammar. But it has to be done only
once (other editors like nvim support tree-sitter grammars trough plug-ins).
In addition to that, there is tooling available that can transform an EBNF
grammar into a tree-sitter grammar. So the hard part does not have to be hard.

### A bit more remapping

`helix` provides shortcuts for moving in a document (in the style of vim-
unimpaired). They allow to quickly move between LSP diagnostics, uncommitted
changes, functions.

Sadly, the default mapping is on `[` and `]`, which are hidden under `AltGr` on
a bépo layout. So I moved them on `(` and `)`, which are available directly.

```toml
[keys.normal."("]
"d" =  "goto_prev_diag"
"D" =  "goto_first_diag"
"g" =  "goto_prev_change"
"G" =  "goto_first_change"
"f" =  "goto_prev_function"
"t" =  "goto_prev_class"
"a" =  "goto_prev_parameter"
"c" =  "goto_prev_comment"
"T" =  "goto_prev_test"
"p" =  "goto_prev_paragraph"
"space" =  "add_newline_above"

[keys.normal.")"]
"d" = "goto_next_diag"
"D" = "goto_last_diag"
"g" = "goto_next_change"
"G" = "goto_last_change"
"f" = "goto_next_function"
"t" = "goto_next_class"
"a" = "goto_next_parameter"
"c" = "goto_next_comment"
"T" = "goto_next_test"
"p" = "goto_next_paragraph"
"space" = "add_newline_below"
```

Like kakoune, helix has contextual menus that display available commands. So
when I type `)`, the list of available commands is displayed. This works for
default and custom bindings. Neat!

So far, that is the _only_ remapping I’ve done to accomodate bépo. Back on vim,
I had `hjkl` bindings remapped to `ctsr` in order to keep movement on the home
row, but the ripple effects and the added cognitive overhead were not worth it.

## Opening a PR

One big missing feature of helix was support for
[dhall](https://dhall-lang.org). Fortunately, there was a tree-sitter grammar
available. So it was just a matter of writing queries for syntax highlighting
and text objects. Easy peasy. (I didn’t do indent queries for now, don’t
hesitate to add them if you know how to do that).

The PR was reviewed and merged in under 3 hours. Neat!

Quite notably, all the features I’m waiting for in `helix` are already in PRs
waiting for review.

- Softwrap <https://github.com/helix-editor/helix/pull/5420>
- Render trailing whitespace <https://github.com/helix-editor/helix/pull/4306>
- Add labels to custom mappings
  <https://github.com/helix-editor/helix/pull/3958>

## What’s next?

Helix has a nice codebase, but since every change I’m looking for is already
waiting in a PR, I don’t have any reason to hack on it. Maintainers on github
and matrix have been nice and helpful, so I’m sticking around.
