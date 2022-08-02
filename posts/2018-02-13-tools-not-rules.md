---
title: "Tools, not rules: why i have built issues-helper"
author: Clement Delafargue
tags: rust, gitlab
canonical: https://www.clever-cloud.com/blog/features/2018/02/13/issues-helper/
---

A few weeks ago I've open sourced [issues-helper](https://github.com/CleverCloud/issues-helper). It allows you to easily interact with issues on GitHub and GitLab, from the command line.

    gli o --assignee clementd --label improvement "Degauss the frobulator fluxfield"

Let's see why and how it's done.

## TL;DR

Issues are a good way to keep track of decisions in a project. Opening issues should be easy so people don't forget to write down why they've done things. Opening issues from the command line with minimal fuss is a good thing:

    gli init # generate initial configuration
    gli o "Issue title" "Optional issue text"
      # add --open to open the issue in the browser once created
      # add --assignee to assign issue
      # add --label to add labels to the issue
    gli l # list issues
    gli b # open the project homepage in your browser
    

## Issues. Use them.

At Clever Cloud, we use git (and GitLab) a lot. In addition to just using git for code, we also rely a lot on issues. Many discussions end with "please open an issue and sum up what we've just said".

Issues are not just for bugs, we use it as a way to track decisions. Some projects don't even have code and are just there to track discussions and decisions.

My goal, as a CTO, is to make sure we keep track of all of our decisions. So I need people to open issues, to open them often, and to open them early.

## Processes. Make 'em easy.

I work with people, not with robots. While I trust them and I know they're good at what they do, I just can't expect them not to make mistakes, or in that case follow processes with no clear incentive. So I needed to remove friction when opening issues. When we interact with code, it's from the command line: we craft commits, we deploy to production, we even close issues from the command line (while crafting commits). So we should also open issues from the terminal.

My first try was to use the ruby gitlab CLI, but it had several issues:

- configuration was done with environment variables
- it's written in ruby and can be complicated to install
- the output is more or less database rows in ascii tables
- you have to specify which project you're talking about

Even though I've written documentation and showed people how to use it, it was not used a lot. When a tool feels like a hack, nobody wants to use it.

### What I wanted

I needed two things to get a tool people would use:

- automatically know which project we're talking about
- config files to avoid having to mess with env variables (and leak GitLab tokens)

To avoid ruby installation issues, I've decided to use [Rust](https://www.rust-lang.org/). There's a lot of available libraries, the build system works well, it's sufficiently expressive to let me express what I want, and in the end I get a nice binary.

#### XDG basedir. Respect it.

Reading GitLab credentials from a file is not really hard. One common mistake is to put config files directly in `~` like some kind of animal. Instead of cluttering users' home directories, I followed the [XDG basedir spec](https://specifications.freedesktop.org/basedir-spec/basedir-spec-0.6.html). Most languages have a library which handles it for you, so it really is a no-brainer (and it works out of the box on Windows as well).

#### Infer information from context

To know where the issues should be opened, `issues-helper` looks at the `origin` remote of the current git repo. From that, it knows what to do. Even if you're not currently in the right repo,

    j my-project # autojump is great
    gli o "My issue" # that's it
    

## Tools. Open Source them.

Once I had this working, not only people started using this tool, but people improved it: [@keruspe](https://twitter.com/keruspe) fixed a lot of issues quite quickly. So the next step was to un-hardcode every Clever Cloud specific things in the codebase (then rebase everything, fun afternoon).

Once it was on GitHub, I got more contributions (from inside and outside). Also, it made me add GitHub support: not being able to use issues-helper to manage its own issues quickly got very frustrating.

Supporting GitHub also made me contribute on the GitHub crate, thus closing a vertuous circle.

There are [a few issues left](https://github.com/CleverCloud/issues-helper/issues). Feel free to chime in, I'll help you get started. It's a good way to start working on a Rust project!

## How it's done

The tool itself doesn't hold a lot of business logic. It parses a project location from a git remote, reads config from a file, sends a few HTTP requests and displays useful information.

### CLI arguments parsing

It's one of the most important parts of a program, since it's the UI. The Rust ecosystem has [clap](https://clap.rs/) which handles everything: parsing, help and even autocompletion.

Unfortunately, the parsed values are exposed through a `Map<String,String>`, so even though parsing checks structure, you have to recreate the structure yourself. In practice, you end up with a bunch of `value.unwrap(); // this should never fail`.

Fortunately, there's [Structopt](https://crates.io/crates/structopt), which generates clap config from Rust data structures: all the dirty stuff is handled for you, and you get nice structs and enums. Yay!

### Config files

Config is in a toml file, config file location is handled by the [xdg crate](https://crates.io/crates/xdg). Remember: every time you add files to `~`, a cute puppy dies.

## So

Having processes in a team is a good thing, but if you don't document them, you can't expect people to respect them. If processes don't make everyone's life easier, you can't expect people to respect them. One simple way to make processes stick is to provide tooling to make them the easiest path. A bash script is cool, but mainly for its author. A stable CLI tool with a good UI, written in a robust language is not a lot of work and will age _way_ better.
