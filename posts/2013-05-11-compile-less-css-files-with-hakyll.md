---
title: Compile Less.css files with Hakyll
author: Clement Delafargue
tags: hakyll, less, lesscss
---

At the [Company Campus](http://companycamp.us) we use
[Less](http://lesscss.org) quite intensively when we develop websites. It
allows us to have cleaner, more maintainable stylesheets. Turns out it's
quite simple to integrate it into Hakyll.

Hakyll provides `unixFilter` which allows to use any command-line utility
during the build. So we can use it to compile `.less` files into `.css` files.

To keep things modular, we split our code into many `.less` files and assemble
it with `@import` rules. The less compiler then generates a unique,
compressed CSS file. Since the dependencies are declared and handled by
`lessc`, hakyll needs a little help to know when to recompile the main less
file:

```haskell

    -- Tell hakyll to watch the less files
    match "assets/css/*.less" $ do
        compile getResourceBody

    -- Compile the main less file
    -- We tell hakyll it depends on all the less files,
    -- so it will recompile it when needed
    d <- makePatternDependency "assets/css/*.less"
    rulesExtraDependencies [d] $ create ["assets/css/main.css"] $ do
        route idRoute
        compile $ loadBody "assets/css/all.less"
            >>= makeItem
            >>= withItemBody 
              (unixFilter "lessc" ["-","--yui-compress","-O2"])
```

