---
title: <code>null</code> is not the issue
author: Clement Delafargue
tags: java,null
canonical: https://www.clever-cloud.com/blog/engineering/2016/07/21/null-is-not-the-issue/
---

Nowadays, we know that `null` is to be avoided. It's been dubbed the _billion dollar mistake_ by its own creator, and the dreaded `NullPointerException` everyone knows about. Yet, when it comes to getting rid of `null`s, nobody agrees.

## `null` is bad, m'kay

- NPEs
- hard to read (is this value always defined?)
- handling possibly undefined values is tedious

The issue is that we focus on `null`, and not on the actual problems it causes. If your only goal is to get rid of NPEs at all costs, then you will pay those costs. Dearly.

The memory safety issues caused by `null` are solved on most platforms. In many languages, `null` is a pointer to a specific value with specific properties, so not literally a null pointer anymore, and on modern OSs, the memory protection system will prevent your code from directly accessing the address `0x00`.

So the big risk is having your program blow up (`NPE`, `segfault`, `panic`, …) because of an unexpected missing value. The core issue is that you were unable to express that the value was possibly missing; the crash is a consequence of this issue. Yet many people try to solve the issue by preventing the crash (null check, [null object pattern](https://en.wikipedia.org/wiki/Null_Object_pattern), …) instead of handling the core issue: a value could be missing and you weren't able to express it.

## Stop chasing NPEs and fix your domain model

If you're trying to get rid of NPEs by removing the difference between "there is a meaningful value" and "there is no value", then not only you're not solving your issue, but you're making it far, far worse.

Your domain model didn't let you express statically the possible absence of a value, but the presence of null at least gave you a stuctural difference at runtime. That's why putting default values or worse, following the null object pattern, is terrible. You had structural information, but couldn't use it in a rigorous way, so you're just throwing it all away because of an implementation detail.

If you're searching for a replacement to `null` to denote missing values, it has to be structurally different from a regular value (it cannot belong to the domain of the value you may have). So if you're in a typed system, then a value that may be not there has to have a different type from a value that's definitely there. In an untyped language, you can't have a static difference, so using `null` is kind of OK (though there are better solutions).

For example, in java you can do:

```java
// Bad
String notThere = null;
String there = "my string";
// Turbo-bad
String notThere = ""; // or "N/A", etc
String there = "my string";
// Good
Optional<String> notThere = Optional.empty();
Optional<String> there = Optional.of("my string");
```

With optional, not only you have a structural difference between defined and undefined strings (instead of having to check if the string is equal to `""` or `"N/A"`, without any guarantee that it's not the actual value), but it's clearly documented in the type.

`Optional` is available in Java 8 (and in Guava if you're not using java 8 yet). In scala, rust, ocaml and many other languages, it's called `Option`, in Haskell it's called `Maybe`.

## This is why you _can_ have nice things

Ok so now you have a proper representation for your optional values. In a typed language it means that you're forced to check if the value is already there before using it.

For instance, in java:

```java
Optional<Integer> parseOptionalString(Optional<String> myOptionalString) {
    if(myOptionalString.isPresent()) {
        Optional<Integer> result = parseInt(myOptionalString.get());
        return result;
    } else {
        return Optional.empty();
    }
}
```

This works perfectly well, but is still as verbose as using explicit `null` checks. So if you're only concerned about code terseness (you really shouldn't), it can feel like a marginal improvement (it's way better than that).

Thankfully that's not the idiomatic way of handling `Optional` values: now that we can denote the abstract concept of _being possibly not there_ (in this case, `Optional<_>`), then we can do useful stuff about it:

- transforming it only if it's defined (with `map`)
- eliminate the option by providing a default value when we don't need the  
- information anymore (with `orElseGet`)
- sequencing several operations returning options (with `flatMap`)

All of this comes for free because we were able to clearly define our domain model. Some languages like Kotlin provide approaching solutions with things like the Safe Call Operator for chaining operations (instead of using `map` and `flatMap`) and the Elvis Operator for providing default values. This, however, is less extensible and less composable than having proper options (`map` and `flatMap` are not specific to `Optional`).

```java
Optional<Integer> parseOptionalString(Optional<String> myOptionalString) {
    return myOptionalString.flatMap(parseInt);
}
```

Once you have properly defined your domain model, then you can have cleaner code thanks to the information encoded in your types. Aiming at terser code without proper abstraction will lead you to perlish nightmares.

## To sum up

Using `null` causes problems. Resorting to solutions that erase information (setting a default value too early or worse, the null object pattern) will cause graver and subtler problems.

In order of decreasing importance, the problems with `null` are:

1. it's not clear for people if a value can be missing
2. the program can blow up if somebody forgot to handle a missing value
3. it's tedious to sequence operations returning null

Don't settle for solutions that only address #2 and #3.
