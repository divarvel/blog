---
title: Snap framework apps over Clever-Cloud
author: Clement Delafargue
tags: haskell, clever-cloud, PaaS
---

## This blog post is out of date

Clever Cloud now has [native support for haskell
applications](https://www.clever-cloud.com/doc/haskell/haskell/).  Any
*stack*-based application will be compiled and launched automatically on Clever
Cloud.

You can watch [how to deploy a scotty app on Clever
Cloud](https://www.youtube.com/watch?v=TnmNoPJdQvk&t=11s).

You can still read the original article below.

Even though my comfort zone, professionally, is Scala + Play Framework for web
applications, I really like Haskell. For some reason, I had never written web
apps in Haskell before last week. The Haskell web landscape has three main
players, Yesod, Snap and Happstack. I've settled on Snap framework.

Since I use [Clever-Cloud](http://clever-cloud.com) to deploy my web
applications, and since it doesn't support cabal builds… yet, I had to cheat a
bit to make it work.

As I said, [Clever-Cloud](http://clever-cloud.com) does not support cabal
builds. But it can run binaries on its scalers, and a compiled snap
application is a standalone binary, with an embedded HTTP server.

## Create the application on Clever Cloud

The trick is to deploy a normal Clever Cloud application, and run the haskell
binary as part as the deployment process. Since node applications are not tied
to an external HTTP server (as do PHP, Ruby and Python apps), and npm provides
a simple way to run scripts during deployment, I'll use a node js application.

Create a node application in the Clever Cloud console.

<http://console.clever-cloud.com>

<a href="/files/create_app.png"><img src="/files/create_app.png" alt="app creation in clever cloud" width="100%" /></a>

Take note of the git remote URL, you'll need it later.

## Setup the repository

Create a new git repository, and put this in a file named `package.json`:

```json
{
  "name" : "my-snap-app",
  "version" : "0.1.0",
  "scripts" : {
    "start": "./my-snap-app -p 8080 --proxy=X_FORWARDED_FOR --no-access-log"
  }
}
```

Make sure to put in the repo your snap binary (found in `/.cabal-sandbox/bin/` after a
successful `cabal install`), the `site_key.txt` file and the `snaplets`
directory which contains configuration files.

The binary must be a 64-bit linux binary. If you're not on a 64-bit linux
system, you have to cross-compile it, or use a monstrously dirty solution
like Boot2Docker if you feel adventurous.

Make sure **not to include** the `log` directory.

## Deploy on Clever Cloud

    git remote add clever <git-url>
    git push clever master

And *voilà*.

## More details, caveats

Upon deployment, `npm start` is launched, which starts the snap binary.
The monitoring makes sure someone listens on port `8080` (the snap binary
does) and marks the application as successfully deployed.

The application can be automatically scaled (both horizontally and vertically)
as long as you don't store anything on the local file system.

### flags

The `-p 8080` is there to instruct snap to listen on port `8080` (required by
clever-cloud).

Since the application is behind a reverse proxy, you need to set
`--proxy=X_Forwarded_For` to have access to the clients' ip addresses (else it
would take the address of the reverse proxy).

Lastly, it's clever-cloud responsibility to store the access logs, so you
should disable it to keep only error logs, hence the `--no-access-log`.

### Caveats

Snap has currently two issues: it logs to files and it reacts strangely to
HAproxy.


#### Don't log to files. Period

When you industrialize your deployment, one of the first things to do is log
aggregation. Instead of logging to files, you forward the log to a collecting
system. Clever cloud does this automatically with everything your application
outputs to `stdout` and `stderr` .

Moreover, an industrialized deployment process deploys fresh version of the
code every time, and all the local changes are lost. That's why logging to
files is bad practice.

 <del>Sort of fortunately, snap does log to `stderr` when it can't find the log
location (default: `./log`). While displaying a big, scary warning. You can
safely ignore it.</del>

 <ins>You can tell snap to log to ``stdout`` or to ``stderr``, by passing
``stderr`` or ``-`` (for ``stdout``) to ``--error-log`` or
``--access-log``.</ins>

#### <del>HAproxy</del><ins>Monitoring</ins> log spamming

 <del>In front of all the Clever Cloud scalers is HAproxy, which does the load
balancing and the fail-over. To make sure the applications are really running
and responding, HAproxy does a regular TCP check.</del> <ins>All applications
on Clever Cloud are monitored (it allows Clever Cloud to automatically restart
applications when they crash). The monitoring system periodically does a
simple TCP check (it opens a TCP connection with the app, then closes it right
away).</ins>

Unfortunately, snap can't really handle it and logs an error. **It's not a big
problem, as <del>HAproxy</del><ins>the moritoring system</ins> still counts
the application as alive**. However, it tends to spam the logs a bit. I don't
have a solution for this yet. Maybe a more experienced snap user knows how to
deal with this.


## Wrap up

It's easy to deploy snap applications on
[Clever-Cloud](http://clever-cloud.com), and you can take advantage of all its
great features: pay-as-you-go, auto-scalability, and great performance.

