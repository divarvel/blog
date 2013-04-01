---
title: Delicious burritos in PHP with PHPz
author: Clement Delafargue
tags: fp, php
---

These last months, I had to work with PHP. The lack of functional constructs
was really a PITA, so I've developped a small library to bring
[burritos](http://en.wikipedia.org/wiki/Monads_in_functional_programming) to PHP.

Now you can enjoy some ivory-towerish programming style while staying in the
real world pragmatic trenches of PHP \\o/.

PHPz brings the power and expressiveness of functional programming to the PHP
masses. It is heavily inspired from scalaz, which also brings functional
constructs to an otherwise imperative language.

Like scalaz, PHPz uses the typeclass pattern to bind new behaviour to existing
constructs.


```PHP
$xs = __t(array("foo", "foobar", "foobarqix"))
            ->map(function($x) { return strlen($x); })
            ->bind(function($x) { return array($x - 5, $x, $x + 5) ; });

print_r($xs());
```

The code is on github: <http://github.com/divarvel/phpz>. Enjoy :)

The next step is the implementation of zygohistomorphic prepromorphisms.
