---
title: Single-page static website generation with Hakyll
author: Clement Delafargue
tags: hakyll, web
---

## Static sites generators

Static sites generators are quite in a hype these days. They bring lots of
advantages over CMSs: security, speed, easier versionning (contents are in the
SCM, not in the DB, use a real text editor, not a crappy WYSIWYG)… With web
servers like Nginx it only gets better.

I've been fond of static site generators for a some time now. I've started
with [rest2web](http://www.voidspace.org.uk/python/rest2web/) which used
docutils to generate webpages. We used it to create the website of our Junior
Entreprise. [Nelle Varoquaux](https://github.com/NelleV) managed to get a nice
site to build but I remember it to be non trivial. It involved putting some
custom python code im my `/lib` and bugging rest2web's creator with a few
emails. I liked the concept but it was a real pain to use and even more to
customize. Nelle did a fine job with it but our successors at the Junior
Entreprise dropped it altogether and replaced it by a hand-rolled PHP website.

Even though the tool support was less than ideal, I got hooked to the idea of
static websites.

Some time later, Jekyll became widely used thanks to GitHub pages and I've
rolled a few websites with it, my company's website, <http://eklaweb.com>
(I'll come to that later), my company's blog <http://blog.eklaweb.com>, and a
few other projects. I've advocated its use for <http://ordify.de>.

Working with Jekyll gave quick results and easy-to-build websites but this way
fell short quite quickly when trying to less standard stuff.

Building <http://eklaweb.com> with Jekyll was not possible out-of-the-box with
Jekyll which is designed to create one HTML file per input file. Since it's a
single-page website, this leads to all kinds of dirty hacks. Generally
speaking, very few projects with Jekyll were achieved without hacking its
blogpost mechanism in Lovecraftian abominations.

I've also tried to use [Scalate](http://scalate.fusesource.org/), a scala
templating *library* which was said to also support static website generation.
Due to how it was packaged, I've never been able to make it work, but it gave
me an interesting vision on static website generation. The key was not to
provide a *library* which lets you build your own generator. Unfortunately
this is not how most static sites generators work. This way of thinking can
more or less be linked to

>More libraries, less frameworks

but that's another story.

## Enter Hakyll

[Hakyll](http://jaspervdj.be/hakyll) is a haskell library which lets you
create your own site generator. For convenience, there are a few examples
provided so you can have a blog running without coding everything, that's what
I did with this blog.

Hakyll uses [pandoc](http://johnmacfarlane.net/pandoc/), so it supports a
[huge](http://johnmacfarlane.net/pandoc/diagram.png) number of formats.

After porting my blog to Hakyll 4, I felt comfortable enough to try to dive
deeper and I tried to recreate my company's website with more modularity, with
the different parts cleanly separated into different templates and markdown
files.

Here's my attempt: <http://github.com/divarvel/hakyll-single-page-test>. Every
section of the page has its own template and the blocks (whose numbers vary in
each section) are in different files.

## How it works

Hakyll is based on a few important types:

- `Compiler a`, which produces a unit *and tracks its
  dependencies*. The types are very generic, I won't be more specific.
  `Compiler` is an instance of `Monad` so you can easily assemble
  compilers to create more powerful ones.
- `Item a` which pairs some content to an `Identifier`
- `Context a` which provides an immutable context allowing to inject data
  in a template. Typically it will contain the file body and its metadata
  (title, date, tags). You can join contexts since they're instances of
  `Monoid`

For a regular multi-page website, you write a compiler which turns every input
file into an output file.

For a single page website, you have to do it a bit differently. You can
compile every input file, but you have to pipe different compilers to assemble
the parts into one page.

In my example, each section contains a title, a description text and a
variable number of blocks, each of which with a title, an image and some metadata.
There is a template for the blocks, and a template for the whole section.

The content files have this structure:

    blocks
    ├── formations
    │   ├── audit.md
    │   └── formations.md
    ├── formations.md
    ├── index.md
    ├── people
    │   ├── clement.md
    │   ├── godefroy.md
    │   └── lefu.md
    ├── people.md
    ├── refs
    │   ├── alixio.md
    │   ├── ecn.md
    │   ├── lapompadour.md
    │   └── rezoto.md
    ├── refs.md
    ├── technos
    │   ├── html5.md
    │   ├── nosql.md
    │   ├── phonegap.md
    │   └── scala.md
    └── technos.md

The templates have this one:

    templates
    ├── blocks
    │   ├── formations.html
    │   ├── people.html
    │   ├── refs.html
    │   └── technos.html
    ├── default.html
    ├── formations.html
    ├── people.html
    ├── refs.html
    └── technos.html

So, how do we assemble it back ?

First, compile all the blocks

```haskell
match "blocks/**.md" $ do
    compile pandocCompiler
```

I've not specified a route, so they won't directly appear in the generated
site. They're just compiled so another compiler can use them.

To compile a section (for instance, the refs section), it's easy.
To keep the code simple, I'll give an example for only one section. In the
final code, it's not duplicated, I just pass an options record which keeps the
whole thing DRY.

First, we assemble the blocks

```haskell
refsCompiler :: Compiler String
refsCompiler = do
    blocks <- loadAll "blocks/refs"
    blockTemplate <- loadBody "templates/blocks/refs.html"
    blockList <- applyTemplateList blockTemplate defaultContext elements
```

`applyTemplateList` applies a template to every element of the list and joins
the result. `defaultContext` is provided by Hakyll and contains the block's
metadata and body.

Then we inject the elements in the section and return its contents (we drop
its `Identifier` since it won't be a page on its own).

```haskell
    sectionTemplate <- loadBody "templates/refs.html"
    sectionData <- load "blocks/refs.md"
    section <- applyTemplate sectionTemplate (sectionContext blockList) sectionData
    return $ itemBody section
```

With `sectionContext` I pass the blocks list to the section template, while
keeping the sections metadata (`Context` is an instance of `Monoid`).

```haskell
sectionContext :: String -> Context String
sectionContext list =
    constField "blocks" `mappend`
    defaultContext
```

The last thing is to combine all those compilers into one page:

```haskell
create ["index.html"] $ do
    compile $ do
        pageData <- load "blocks/index.md"
        r <- refsCompiler
        page <- loadAndApplyTemplate "templates/default.html" (indexContext r) pageData
        makeItem $ itemBody page
```

As with `sectionContext`, `indexContext` just adds the section contents to
`defaultContext`.

```haskell
indexContext :: String -> Context String
indexContext r = constField "refs" r `mappend` defaultContext
```

The important parts are here. You also have to compile CSS, static files and
this sort of stuff, but it's quite boring.

Once you have everything, juste generate your generator (so meta) and build
your site:

    $ ghc --make hakyll.hs
    $ ./hakyll build

You can also launch a preview server with automatic rebuild:

    $ ./hakyll preview

Or check your links to find broken links:

    $ ./hakyll check

Hakyll provides simple yet powerful building blocks which allow to build
anything you want without resorting to ugly hacks, but keep in mind that
you're not forced to write all the code by yourself. There are examples for
common use cases: a static site, a blog with tags, RSS / Atom support,
multi-lang setups… The point of this post was that it's possible to build
something *really* custom with it.

Hakyll is fairly well documented and there are a few hakyll-based websites
whose source is available:

- [Examples](http://jaspervdj.be/hakyll/examples.html)
- [Tutorials](http://jaspervdj.be/hakyll/tutorials.html)

Enjoy, and don't forget to thank Jasper on `#hakyll` (freenode).
