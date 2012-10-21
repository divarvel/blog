---
title: Blog deployment system
author: Clément Delafargue
tags: misc
---

I've spent some time to have an automatic deployment system for this blog.
I'll explain briefly how it works.

## Bricks

This blog is statically generated with Hakyll. The generated files are placed
in `_site` (which is `.gitignore`'d).

The hosting platform is [Clever Cloud](http://clever-cloud.com) which offers
git deployment.

Since the generated are not versioned, I can't directly push it.
For that, I need to create a new empty, orphan branch :

    git checkout --orphan publish

This branch will now hold the generated files.

So every time I publish something, I have to :

* generate a clean site
* checkout to `publish`
* remove the source files and copy the generated files
* commit
* push
* get back to `master` and clean up the mess

Plus a few extra (save the current state to a stash during the deployment, for
instance).

All of which conveniently wrapped in a `make publish`.
Feel free to read and comment the source code (which is quite dirty, I know).
[Makefile](https://github.com/divarvel/blog/blob/master/Makefile)

There are a few ToDos left, such as tagging and signing every publication.
