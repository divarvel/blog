---
title: My everyday exherbo commands
author: Clément Delafargue
tags: sysadmin, exherbo
---

I've been using Gentoo for a few years. Switching to a source-based distro has
been a pleasure. It may seem a bit overkill but even with binary distros,
recompiling stuff is quite common. Recompiling software outside of the package
manager is a pain which I don't want to suffer again.

Gentoo's package manager, *portage*, is quite nice to use and offers advanced
features (for instance, the slots allow to have both ruby 1.8 and 1.9
installed on your system, no rvm \o/).

Nevertheless, portage has been less and less usable on my machine. Its
dependency on python makes it break fairly often and its centralized nature
made it slower over time.

That's why I switched to paludis, an alternative package manager written in
C++. Paludis devs have created Exherbo, a source based distro with paludis as
its main package manager. The repositories are fully decentralized and there
is no difference between users and contributors.

I've been building my exherbo gradually over the last few months and it's now
fully usable. The base system is really lighter than Gentoo's, making fewer
assumptions about your choices (eg it does not provide a default init system).
If your system is quite customized, it's really an interesting distro. Since
everybody can provide its own repo, it's easy to provide the needed config
files. For instance, systemd has a really nice support. Basically, this distro
is optimized for upstream contributions.

The community is nice but has a strong RTFM culture. So you need to make sure
to research your issue thoroughly before asking for help.

##Updating the system

    cave resolve -c1 -Cs -km -Km world

Followed by an optional

    cave fix-linkage

Updating the config files can be done with

    eclectic config interactive

Sometimes I have small issues with Perl libraries not properly updated after a
perl update, so

    cave resolve nothing --reinstall-dependents-of perl -W ghc

Will trigger a reinstall of everything which depends on perl (hopefully the
perl libraries). If there are big packages depending on perl you don't want to
reinstall (say, ghc, which takes 2 hours to build on my machine), the ``-W``
flag can help you.

Sometimes, a package's name changes, and it can confuse the resolver (it
creates a dependency cycle between the old and the new name).

For instance, ``libelf`` was recently renamed to ``elfutils``

    cave resolve nothing --reinstall-dependents-of libelf \!libelf

