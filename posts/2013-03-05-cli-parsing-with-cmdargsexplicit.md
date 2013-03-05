---
title: CLI parsing with CmdArgs.Explicit
author: Clement Delafargue
tags: haskell, hammertime, fp
---

As a CLI program, hammertime's UI is command-line flags and arguments. The
first iteration was basic pattern matching on the result of `getArgs`. Then
[Julien](http://twitter.com/jutanguy) refactored it with `CmdArgs`, a very
powerful library which allows to create command-line parsers in a very
declarative way. It features a multi-mode option (a bit like `git add`, `git
commit`). It's very easy to declare subcommands, each with its own set of
options. It automatically creates a nicely formatted `--help` output.

Unfortunately, the default (and most documented) way of declaring the options
is impure and uses untracked side-effects, which is generally frowned upon in
the haskell community. It's also not very typesafe and the parsing easily
breaks at runtime. The `CmdArgs.Implicit` module also uses lots of `Typeable`
dark magic to auto generate options from record fields. The CLI is tightly
coupled to the way you name your data structures. For all these reasons, I've
decided to rewrite the CLI from scratch, using `CmdArgs.Explicit`, a pure,
non-magic CLI parser generator. The use of `CmdArgs.Explicit` is way less
documented than `CmdArgs.Implicit`, so here's how I did it.

##Data structures

The first step is to model what the user asked with a data structure. For
hammertime, the user can tell:

- I've started an activity
- I've stopped the current activity
- Show me a report (with some filters)
- Show me some help
- Show me your version number

```haskell
data Action = Start { project :: String
          , name :: String
          , tags :: [String]
          }
            | Stop
            | Report { span_ :: Types.TimeSpan
                     , project_ :: Maybe String
                     , name_ :: Maybe String
                     , tag_ :: Maybe String
                     , type_ :: Types.ReportType
                     }
            | Help
            | Version
            deriving (Show)
```

The underscores in the name fields is because of name conflicts. Haskell's
records are quite annoying when it comes to namespacing.
The `deriving (Show)` is not crucial but quite useful for debugging.

##Declaring modes

The `Start`, `Stop` and `Report` actions are subcommands. `Help` and `Version`
are triggered by flags (`-?`/`--help` and `-V`/`--version` respectively).

The `Stop` mode is the easiest. It does not take any arguments nor flags.

```haskell
stopMode :: Mode Action
stopMode =
    let m = mode "stop" Stop "Stop current activity" dummyArg []
    in m { modeArgs = ([], Nothing) }

dummyArg :: Arg a
dummyArg = flagArg (\_ _ -> Left "") ""
```

