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

You should read the following with [the documentation](http://hackage.haskell.org/packages/archive/cmdargs/0.10.1/doc/html/System-Console-CmdArgs-Explicit.html) and
[hammertime's main](https://github.com/divarvel/hammertime/blob/master/src/Hammertime/Main.hs)
in tabs somewhere.

## Data structures

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

## Declaring modes

The `Start`, `Stop` and `Report` actions are subcommands. `Help` and `Version`
are triggered by flags (`-?`/`--help` and `-V`/`--version` respectively).

### Report mode

The report mode is the easiest to declare.

```haskell
reportMode :: Mode Action
reportMode = mode
                "report"
                defaultReport
                "Generate report for a given time span (default: day)"
                (flagArg setTimeSpan "month|week|day")
                [ flagReq
                    ["project", "p"]
                    setProjectFilter
                    "PROJECT"
                    "Filter by project"
                , flagReq
                    ["activity", "a"]
                    setActivityFilter
                    "ACTIVITY"
                    "Filter by activity"
                , flagReq
                    ["tags"]
                    setTagsFilter
                    "TAGS"
                    "Filter by tag"
                , flagReq
                    ["type", "t"]
                    setReportType
                    "SIMPLE|TOTAL"
                    "Report Type (default: simple)"
                ]
```

The `Mode Action` type simply states that it will extract an `Action` from the
arguments. `Mode a` is just a record. There are different constructors for
different kinds of modes (`modeEmpty` for an empty mode, `mode` for a regular
mode or `modes` for subcommands).

Here we have a simple mode, so we'll use `mode`:

- `"report"`: the subcommand
- `defaultReport` the base value (will be modified by the flags)
- `"Generate report …"` help text
- `(flagArg setTimeSpan "month|week|day")` the argument handler
- `[ flagReq … ]` the flags

The flags act as modifiers. The mode has a start value, which is then modified
by flags. Thus the flags take a `Update a` value which takes the value given
to the flag and updates the generated value.

```haskell
type Update a = String -> a -> Either String a
```

Every flag has a required value, so we'll use `flagReq`.

```haskell
flagReq ["project", "p"] setProjectFilter "PROJECT" "Filter by project"
```
```haskell
setProjectFilter :: Update Action
setProjectFilter p r = Right $ r { project_ = (Just p) }
```

We allow both `--project` and `-p` to be used. `setProjectFilter` updates the
`Report` record with the given project name.

Anonymous arguments are like flags, they take a value and update the generated
action.

Here `(flagArg setTimeSpan "month|week|day")` allows the user to choose the
time span used to generate the report.

`setTimeSpan` updates the `Report` record with the right `TimeSpan` value.

### Stop mode

The `Stop` mode is a bit different. It has no argument, so we'll need to
slightly alter the value constructed with `mode`.

```haskell
stopMode :: Mode Action
stopMode =
    let m = mode "stop" Stop "Stop current activity" dummyArg []
    in m { modeArgs = ([], Nothing) }

dummyArg :: Arg a
dummyArg = flagArg (\_ _ -> Left "") ""
```

The only non-obvious part is `dummyArg`. A subcommand can take a various
number of anonymous arguments. `mode` allows to specify one. Here, we don't
want any argument, so we construct the mode with a dummy argument and get rid
of it just after. We also could have used `modeEmpty` to construct the mode
only with record updates, but doing it with `mode` is shorter and easier to
read.
Anonymous arguments are like flags, they take a value and update the generated
action.
To handle multiple arguments, the mode's arguments handler is
`([Arg a], Maybe (Arg a))`.
The `[Arg a]` is for required arguments, the `Maybe (Arg a)` for optional arguments.

```haskell
stopMode =
dummyArg :: Arg a
dummyArg = flagArg (\_ _ -> Left "") ""
```

Just to make sure we don't forget a `dummyArg` somewhere, we make it fail
every time.

### Start mode

The start mode is a bit more complicated, because it can take a various number
of arguments (at least two, project and activity name, then some tags).

```haskell
startMode :: Mode Action
startMode =
    let m = mode "start" (Start "" "" []) "Start a new activity" dummyArg  []
    in m { modeArgs = ([
        (flagArg setProject "PROJECT"),
        (flagArg setActivity "ACTIVITY")
    ], Just (flagArg addTag "[TAGS]")) }
```

`setProject` and `setActivity` just set the project and activity with record updates.
`addTag` appends its argument to the tag list.

```haskell
addTag :: Update Action
addTag t s = Right $ s { tags = (tags s ++ [t]) }
```

## Putting everything together

Now we have all of our submodes, we just have to put everything together (and
add the version and help flags).

```haskell
hammertimeModes :: Mode Action
hammertimeModes =
    let m = (modes
                "hammertime"
                 defaultReport
                 "Lightweight time tracker"
                 [startMode, stopMode, reportMode])
        helpFlag = flagHelpSimple $ const Help,
        versionFlag = flagVersion $ const Version,
        addFlags m' = m' { modeGroupFlags = toGroup [helpFlag, versionFlag] }
    in addFlags m
```

Now you should be able to create your CLI with `CmdArgs.Explicit`. Since it's
pure and without side effects, it's testable and it won't break at compile
time :).
