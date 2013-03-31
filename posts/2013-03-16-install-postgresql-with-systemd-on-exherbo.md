---
title: Install postgreSQL-9.2 with systemd on exherbo
author: Clement Delafargue
tags: exherbo, postgresql, sysadmin
---

A few weekends, I had to play with postgreSQL on my Exherbo. The package was
installed, but Pg needs some post-install init to run correctly. Since it's
not handled by the exheres, you've got a few things to do by yourself.

This is really important as it allows you to configure everything before
actually starting the init process.

## Install

Check the options, namely `systemd` but if you're using systemd, it should be
already enabled. There are a few pg-specific options, like `hstore` which can
be of interest.

    cave resolve postgresql

## Systemd tweaking

PostgreSQL 9.2 dropped some options (namely `silent_mode`), which are still
present in the provided systemd service file.
[Kévin Decherf](http://kdecherf.com/) and I tweaked the exheres a bit to make
it support different service files, one for each version.

The patch is submitted, but not yet merged. In the meantime, you just have to
edit your installed service file by hand.

# Init

Then, the init part (as root).

    mkdir -p /var/lib/postgresql/data/
    chown -R postgres:postgres /var/lib/postgresql
    su - postgres
    initdb -D /var/lib/postgresql/data/

Now everything should be ok.

    systemctl start postgresql

