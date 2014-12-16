---
title: Build up your monad intuition… in JS
author: Clement Delafargue
tags: fp, monad, javascript
---

Most people start to grasp monads with the so called "wrapped metaphor". It
works well with ``List``, ``Either a`` and ``Maybe``, but it can become a
problem with other monads, like ``IO`` for instance. If you have a wrapped
value, why wouldn't you be able to extract it?

A more general intuition could be "computational context which encodes
sequentiality"… which doesn't make sense until it does.

I've found the best way to get rid of the intuition of wrapped values is by
looking at how the continuation monad works. After all, it's [The Mother of
all Monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html).

A few days ago, I came upon an article about [Monads in
PHP](http://blog.ircmaxell.com/2013/07/taking-monads-to-oop-php.html) by
[Anthony Ferrara](https://twitter.com/ircmaxell), which was quite interesting
(I have a sweet spot for [monads in
php](http://blog.clement.delafargue.name/posts/2013-04-01-delicious-burritos-in-php-with-phpz.html)).
Unfortunately, the implementation proposed in the article had two issues:

 - ``fmap`` and ``bind`` are conflated
 - the monad interface has an ``extract`` method

Tweets being a very ineffective means of communication, I came up with a bit
of javascript to illustrate my point. As a side effect it's a nice way to play
with the essence of monads, even if you don't know Haskell.

**Please note you need a basic understanding of monads to play with it.**
Else, it could be a bit daunting.

[Implement the continuation monad in JS](https://gist.github.com/divarvel/7adfa6779eda568a0f28)

[A solution](https://gist.github.com/divarvel/d638c12edc335838f7da)
