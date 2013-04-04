---
title: web2day - powered by Hakyll - part 1
author: Clement Delafargue
tags: hakyll, web2day
---

This post is the first part of **web2day - powered by Hakyll**.

Yesterday, the site for the Web2Day 2013 was released. It's by far the
biggest, most complex website I've built with
[hakyll](http://jaspervdj.be/hakyll).

Kudos to the team:

 - [Quentin Adam](http://waxzce.org)
 - [Pierre Dickinson](http://mecanographik.fr)
 - [Philippe Destré](http://webelis.fr)

This site is different from what I had done with hakyll in many regards, where
it's roughly `1 markdown file -> 1 html file`, plus some indexing (index,
tags, …). In this case, one markdown file can be linked to many others, and
the metadata play a great role. Some pages pull content from other input files
at compile time. In fact, I had to split the haskell in four files to keep it
readable.

I've had a lot of fun building this site and I've learned a lot in the
process. I'll cover what I've learned in a series of articles.

Basic knowledge of Hakyll is needed if you want to understand everything.
You can read the [hakyll tutorials](http://jaspervdj.be/hakyll/tutorials.html)
to gain a basic understanding of how things work.

## The structure

The web2day is built around a series of events. Each event has a topic and one
or more speakers.

The site has one page per speaker (+ one page listing all speakers), one page
per event (+ one page listing all events) and one page per topic (+ one page
listing all topics.

The page about a speakers lists the events he's part of, the page about an
event lists its speakers and topic and the page about a topic lists its events.

Additionally, there are a few standalone pages (partners, general
information…), a blog part (articles + index) and a contributors part (pages +
index).

There are standalone blocks shared on several pages. It'd be too cumbersome to
inject them from the haskell, so I've created a helper function called
directly from the templates.

All the content is both in French and English. The French content is at the
root of the generated site, the English content is in `/en`.

## How it's done

The main `site.hs` file contains only rules definitions. Content-generating
functions are in `RouteFactories.hs` and `LinkedCompilers.hs`. There is a
`Utils.hs` file for unrelated generic functions.

### Languages

#### Keeping it DRY

The content is split between `/fr` and `/en`. The available langs are in an
array and `fr` is stored as the default language. Since the content is
generated in the same way from French and English, let's abstract it away:

```haskell
forM_ langs $ \lang ->
    match (fromGlob $ lang ++ "/blocks/*.md") $ do
    compile pandocCompiler
```

One thing to note: the pattern isn't a string literal anymore, so
``OverloadedStrings`` is not able to turn it into a `Pattern`, you have to do
it yourself.

#### Putting fr at the root.

By default, French content would be put in `/fr`. To lift it to the route, I
use `gsubRoute`:

```haskell
langRoute = gsubRoute (defaultLang ++ "/") (const "")
```

I just have to compose the routes like so:

```haskell
route $ (setExtension "html") `composeRoutes` langRoute
```
#### Switching languages

Assuming that the title of the pages are the same in French and English (big
assumption, I know), I just have to remove the leading `/en` (if the page is
in English) or add `/fr` (if the page is in French).

### Abstracting over common structure

Contributors and blog posts share the same structure: a page for every item
and an index page which lists everything.

For instance, for the contributors

 - every file is compiled to `{/,/en}/contributors/<name>.html` with the
   template `contributor.html`
 - the index page is generated to `{/,/en}/contributors.html` with the
   template `contributors-page.html`
 - every item is the index page is generated with the template
   `contributor-item.html`

With the help of a few naming conventions, it's easy to abstract it away, and
only provide `contributors` and `contributor`. The rest is automatically
derived:

```haskell
makeElementsWithContext ctx plural singular lang = let
        bigCtx = ctx `mappend` (globalContext lang)
    in
        match (fromGlob $ lang ++ "/"++ plural ++"/*.md") $ do
            route $ (setExtension "html") `composeRoutes` langRoute
            compile $ pandocCompiler
                >>= loadAndApplyTemplate (
                    fromFilePath $ "templates/"++ singular ++".html") bigCtx
                >>= loadAndApplyTemplate "templates/default.html" bigCtx
                >>= relativizeUrls

makeElements = makeElementsWithContext mempty

makeIndexPage plural singular lang =
    create [fromFilePath (lang ++ "/"++ plural ++".html")] $ do
        route $ (setExtension "html") `composeRoutes` langRoute
        compile $ do
            let elts =
                    field plural (\_ -> elementList lang plural singular) `mappend`
                    globalContext lang
                tplName = fromFilePath ("templates/" ++ plural ++ "-page.html")

            makeItem ""
                >>= loadAndApplyTemplate tplName elts
                >>= loadAndApplyTemplate "templates/default.html" (globalContext lang)
                >>= relativizeUrls
```

The ability to provide a custom context will be useful for the speakers,
events and topics pages whose contents vary widely.
`globalContext` is a minimal context which contains lots of additional
information, I'll cover it later.

So in site.hs, everything is tidy:


```haskell
--------------------------------------------------------------------------------
-- Posts
--

    forM_ langs $ makeIndexPage "posts" "post"

    forM_ langs $ makeElements "posts" "post"

--------------------------------------------------------------------------------
-- Contributors
--

    forM_ langs $ makeIndexPage "contributors" "contributor"

    forM_ langs $ makeElements "contributors" "contributor"

```

Next time, I'll talk about how the standalone pages and the shared blocks work.
