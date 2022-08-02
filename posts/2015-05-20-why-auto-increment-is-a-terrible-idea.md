---
title: Why auto increment is a terrible idea
author: Clement Delafargue
tags: sql, uuid
canonical: https://www.clever-cloud.com/blog/engineering/2015/05/20/why-auto-increment-is-a-terrible-idea/
---

_This post is from 2015 and may be outdated. I did not chose the somewhat inflammatory title that was decided by marketing, but
I decided to keep it here for consitency. Using UUIDs v4 as primary keys in postgresql does have an impact on index fragmentation.
There are more recent alternatives like ULIDs or UUIDs v6 that aim to provide better index locality while retaining benefits from
UUIDs v4. That being said, this is only an issue on big datasets, so don't worry too much about this if you have less than a million
rows._

As big users of PostgreSQL, we had the opportunity of re-thinking the idioms common in the world of relational DBs.

Today, I'll talk about why we stopped using serial integers for our primary keys, and why we're now extensively using Universally Unique IDs (or UUIDs) almost everywhere.

## TL;DR

Use UUIDs as primary keys. They can be freely exposed without disclosing sensitive information, they are not predictable and they are performant.

## Primary keys, technical keys and semantic keys

A relational database is a graph where nodes are called entities and edges relations. To be able to express a relation between entities, we need to be able to uniquely refer to any entity: that's the role of a primary key.

The role of a primary key is to provide a stable, indexable reference to an entity. You have two ways to construct your primary key: either as a _semantic_ key, or as a _technical_ key.

A _semantic_ primary key is extracted from the entities attributes (ie you use one or several fields of your entity as its primary key).

A _technical_ primary key is completely unrelated to the fields of its entity. It's constructed when the entity is inserted in the DB.

Ideally, semantic keys would be better, as it allows for a very simple way to understand and compare entities. But relational DBs are built upon a fundamentally mutable model. The current state of the world is stored in the database, and entities are subject to mutation. Relational DBs are **very** good at storing a _relatively_ small amount of data, and are not well suited for immutable databases.

That's why to be able to have **stable** primary keys, we commonly resort to _serial IDs_. Serial IDs are a technical key, as they're not related to the actual contents of its entity.

To be fair, you can configure your foreign key to have `ON UPDATE CASCADE` to have primary key updates automatically propagated everywhere within the database, but it doesn't solve the problem if you've exposed primary key values outside the DB.

### What's a serial ID?

Basically, a serial id is a number that increases everytime you insert a row. The database does the bookkeeping for you, so it's transparent for the developer.

    create table entity(
      entity_id serial primary key,
      attribute text not null
    );
    

Copy

We can then insert values in the table; postgres will handle the `entity_id` field.

    > insert into entity (attribute) values ('ohai');
    INSERT 0 1
    
    > select * from entity;
        entity_id | attribute 
    -----------+-----------
                1 | ohai
    (1 row)
    

Copy

The row has been inserted, and the entity has an automatically attributed id. In postgresql, `serial` is implemented through a _sequence_, which acts a bit like a counter. We can inspect its current state:

    > select currval('entity_entity_id_seq'::regclass);
    currval 
    ---------
            1
    (1 row)
    

Copy

OK, so we have a perfectly good primary key for our entities. What's the problem?

## Why serial IDs can be a problem

### Information disclosure

The current value corresponds to the last value used as a primary key. Since the sequence starts at 1 and is incremented 1 by 1, `currval` roughly corresponds in our case to the number of rows in the table (not exactly, because of deletions).

So, if you want to count the number of rows in a given table, all you have to do is to have the system generate a new entity and inspect its ID.

So, do users have access to primary keys? The answer is often yes: primary keys are very often exposed to the user, most often in URLs, as we want URLs to be stable (see [cool URIs don't change](https://www.w3.org/Provider/Style/URI.html)).

Let's say you're a company operating a SaaS, **all it takes to know your user count is to create an account**. A number of social networks boasting impressive user bases were pwn3d this way. Just by creating an account and looking at a link.

### Entity enumerations

An even bigger problem is that it's very easy to enumerate the entities in any given table. You start from 1, and you keep on incrementing the value. It becomes very easy to scrape all your entities. It leads to spamming (just don't start from 1, as you don't want to spam the admin team 😉 ), or worse. If you have light access control, only based on the knowledge of a given URL, then anybody can display private information.

### Non uniqueness across tables

Since each table has its own sequence, an identical value will be found as the primary key of different entities. Now imagine you make a tab-completed typo when deleting a row. Yeah, you've just deleted an arbitrary row somewhere in your DB. Let's hope you don't have too many `ON DELETE CASCADE` configured.

## Workarounds

You can configure your sequence to start at an arbitrary point, have an increment bigger than one, or even share the sequence between tables. In any case, you're still disclosing information about your growth, and you'll just reach integer overflow quicker. Not really a perfect solution.

## A word on UUIDs

You may already have seen stuff like `c0b656b1-7351-4dc2-84c8-62a2afb41e66` somewhere. This is a UUID (or _Universally Unique IDentifier_).

Which is nice. UUIDs are guaranteed to be _Universally Unique_, which makes them good candidates for primary keys (and neatly solve the issue of having primary keys non unique across tables).

More precisely, UUIDs are 128 bits values, with textual representation in hex digits. It bears repeating, because many people think that UUIDs are stored as text.

### UUIDs are 128 bit values

There are different versions of UUIDs, based on how they're created. Within the 128 bits, 4 are used to encode the version. The kind we're interested in is UUIDv4, which is based on randomness.

### UUIDv4 are perfectly well indexed by PostgreSQL

### UUIDv4 values are random

UUIDv4 values being random, you don't have a guaranteed uniqueness. However, the probability of a collision is rather small (see [Random UUID probability of duplicates](https://en.wikipedia.org/wiki/Universally_unique_identifier#Collisions)).

Also, keep in mind that in the extremely unlikely case (you're more likely to get hit by a meteorite) of colliding UUIDs, it will be caught by the DB thanks to the primary key constraint.

UUIDv4 adds two reserved bits to the already used ones, so we're left with 122 bits of information.

```
λ> 2 ^ 122
5316911983139663491615228241121378304
```

This should be enough for your needs.

The fact that UUIDv4 values are random gives us interesting properties:

- you can't enumerate values
- you don't know how many values are present in each table
- you don't have to talk to the database to construct a value

The two first properties are nice, scroll up if you don't remember why. The third one deserves some extra explanations.

Sequences are mutable state, you need atomicity on get-and-increment, to preserve uniqueness. So everytime you need to insert an entity, you need the DB to give you its primary key. The entities in your code are incomplete until you talk to the DB. Since a sequential key is technical, not semantic, this can usually be handled by your ORM (or data mapper, ORMs are bad, m'kay?). When inserting several related entities, this can cause additional problems, because you're not only lacking the primary keys, but also the foreign keys too, which are semantic, not just technical.

The UUIDv4 generation algorithm is well documented and available virtually anywhere, so you can generate whole entities without ever talking to the database. This makes the code simpler and clearer as you don't have to handle partially-created entities, and you can test your entity-generation code without having to talk to a **giant blob of mutable state over the network**.

Also, `serial` is a 32 bit integer. You'll reach overflow at some point. Debugging it and migrating a production database to 64 bit integers is an interesting experience, but maybe you can take my word for it and avoid it altogether. With UUIDs it's a problem you just don't have.

Also, if you're brave enough to run a SQL database in a multi-master setup (please, please, please, DON'T), then you're relieved from the shenanigans of having conflict avoiding sequence generation. But please don't run SQL databases in multi-master setups. I have done that: one star, would not recommend.

## UUIDs in PostgreSQL

PostgreSQL supports UUIDs, through the type `uuid`.

```
> select '697dffc9-8ba5-410c-b677-227475a73530'::uuid;
uuid                 
--------------------------------------
    697dffc9-8ba5-410c-b677-227475a73530
(1 row)

```

By default PostgreSQL doesn't have the ability to generate UUIDv4 values. You need the `uuid-ossp` extension for that. From `9.4`, UUIDv4 generation is also provided by the `pgcrypto` extension.

```sql
CREATE EXTENSION "uuid-ossp";
-- or CREATE EXTENSION "pgcrypto";
create table entity(
  entity_id uuid primary key default uuid_generate_v4(),
  -- or entity_id uuid primary key default gen_random_uuid(),
  attribute text not null
);
```

```
> insert into entity (attribute) values ('toto');
INSERT 0 1
> table entity;
                entity_id               | attribute 
--------------------------------------+-----------
    b2a3d43b-5b81-4127-be29-d849de607d78 | toto
(1 row)
```

You can use UUIDs without the `uuid-ossp` extension, but you will have to provide UUIDs when inserting entities (which is a good thing to do anyway).

## When UUIDs are not what you need

If you don't have mutable state, then you don't need a technical primary key, and you can use a semantic key.

If you have strong storage constraints, UUIDs can cause a problem, since they use 2 to 4 times as many space as a serial or big serial. So if you have strong size constraints **and if you don't expose the primary keys**, then UUIDs may be overkill. Also, UUIDs being random, you lose locality, and your index ends up scattered. This causes a definite performance hit when confronted to high insert rates. Also, this creates extra disk use.

In my experience, UUIDs are a safe default. You can question it when you have strong performance constraints and a safe environment.

## How to generate UUIDs in `$LANGUAGE`

### Java, Scala

It's in the standard library.

```
java.util.UUID.randomUUID
```

### Node.js

```
npm install uuid
```

```javascript
var uuid = require("uuid");

uuid.v4();
```

### PHP

Install `ramsey/uuid` from composer.

```php
$uuid4 = Uuid::uuid4();
```

### Haskell

Install `uuid` from hackage

```haskell
uuid :: IO UUID
uuid = nextRandom
```
