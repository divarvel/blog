---
title: Declarative CLI parsing in JS
author: Clement Delafargue
tags: javascript
---

I'm currently working on a CLI application written in javascript. As with any
kind of programs, the UI plays a very important part.

A good application is consistent and easy to discover by its users. The two
means of achieving that with a CLI app are proper help output and
autocompletion.

While there are lots of cli parsing libraries available in node js, very few
of them handle help correctly, and none of them provide any help regarding
autocompletion.

On the other hand in the Haskell world, we have `optparse-applicative` which
makes this problems disappear: it allows you to declare your parsers in a
declarative and composable fashion, generates all the help output and even
manages shell completion.

## CLI Parse

That's why I've released [CLI Parse](https://npmjs.com/package/cliparse). It
does all this (and a bit more).

The code is available: [CLI Parse on github](https://github.com/divarvel/cliparse-node).

```bash
$ npm install cliparse
```

```javascript
var testCli = cliparse.cli({
  name: "testCli",
  description: "Simple CLI written for the sake of the example",
  args: [ cliparse.argument("value") ],
  helpCommand: false,
}, function(x) { console.log(x.args[0]); });
```

Here's a laundry list of nice features:

 - out of the box bash and zsh completion
 - easy-to-manipulate parsers
 - nested commands syntax (*à la* git)
 - custom data parsers
 - custom autocompletion

Since the application is described in a pure data structure, you can
manipulate it as you want (you can even let your users extend your app, like
git, it will be fully integrated, even in the autocompletion).

### Todo-list

It's already usable, but as always there is always room for improvement:

 - native ZSH completion
 - variadic arguments
 - cleaner help output

Help / PRs more than welcome. The code is quite easy to follow, so don't be
shy :-)

### Hear me rant (in French)

I've given a talk about CLI Parse in February: [slides (English)](http://clementd-files.cellar-c1.clvrcld.net/blog/cli-parse-ht.html)

<iframe width="560" height="315" src="https://www.youtube.com/embed/QHbMMNkw2Mk" frameborder="0" allowfullscreen></iframe>


