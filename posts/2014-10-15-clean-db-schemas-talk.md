---
title: Clean DB schemas talk
author: Clement Delafargue
tags: talk, sql, database
---

I gave a quick talk about DB schemas yesterday during the October Human Talks
session.

## tl;dr:

 - Design with querying in mind
 - Have a look at Normal Forms
 - don't let the ORM write the schema
 - use postgresql fully
 - consider using UUIDs as PKs
 - don't fear joins because of MySQL poor implementation
 - be consistent in PKs and FKs naming
 - fields should be not null by default
 - use enums
 - use the expressive types available in PGSQL
 - give jDbT a try: <https://github.com/divarvel/jdbt>

[François Guillaume](https://twitter.com/fgribreau) added an interesting point: in normal operation, don't
actually delete data, mark it deleted instead.

<iframe width="700" height="600" src="/files/embedder.html#db-schemas.html" allowfullscreen />

Download the [slides in HTML](/files/db-schemas.html).

The [source files](https://github.com/divarvel/db-schema-talk) are on GitHub

