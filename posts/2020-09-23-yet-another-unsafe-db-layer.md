---
title: Yet another unsafe DB layer
author: Clement Delafargue
tags: haskell
canonical: https://tech.fretlink.com/yet-another-unsafe-db-layer/
---

_Edit 2022-08-01: My excellent former colleague Hécate found out (after leaving Fretlink) that those internal helpers were actually useful and reimplemented them in an open-source library: [pg-entity](https://hackage.haskell.org/package/pg-entity)._

![yolo](/files/yolo.jpg)

> Photo by [Katie Barkman](https://www.flickr.com/photos/134208113@N08/20446347979)

> "Should I use an ORM?" - the greatest thread in the history of forums,
> locked by a moderator after 12,239 pages of heated debate,  
> @dril (à peu près)

## Disclaimer

First things first, this is not an _ORMs suck_ post. This is more about 
writing down tradeoffs I make time after time when it comes to DB layers.

I find myself making similar choices in different settings, and some of them may not be common practice. Everything here is about tradeoffs informed with my personal experience, so they may not align with your experience and might leave you horrified. _That's okay_, it does not mean we have to fight about it. Just try to keep an open mind.

Even though this is published on my employer's blog, it's mostly personal opinions. I know some are shared by some colleagues, but not all of them.

## What I care about

I'll start by stating my preferences about DB layers:

- I'm most comfy with relational DBs, PostgreSQL in particular
- DB access is central in a service, in terms of correctness and performance
    - thus, I want to be able to easily reason about it
    - thus, the DB schema is _my_ concern
    - thus, transaction boundaries have to be explicit
    - thus, the actual queries emitted have to be obvious
- ideally, types should help me reason about it
- I should be able to test things in isolation as much as possible
    - thus, I should avoid having values created by the DB itself
    - thus, I avoid auto-increment if I can and prefer UUIDs for PKs
    - thus, I avoid using `now()` if I can and prefer generating dates in code
- it's all about tradeoffs, not hard rules. I try to keep things consistent, but I have no problem breaking one of the rules above if necessary

There is commonly found advice about DB layers that I don't value that much. I understand where it comes from and why people advocate it, but my personal experience lead me to rank them lower than what's above. Not that they're bad _per se_, but that their benefits are not always balancing their drawbacks, _as far as I'm concerned_.

- SQL queries should be constructed in a type-safe way  
  _either type safe layers don't follow SQL semantics, obscure the actual
  queries, or are downright tedious to use_
- SQL concerns should not leak to higher layers  
  _transactions should be explicit and not buried, many services are CRUD-like and a proper distinction is not always worth it_

## Where does that leave me now

Based on all of these concerns, I most often end up using very thin DB layers: in Scala I used anorm, and in Haskell I use [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple). I like them because they don't make assumptions for me and let me do what I want. Which means that used unwisely, they give rise to unmaintainable heaps of code (look at me, saying that I'm happy using sharp tools and saying _you just have to use them correctly_).

For those who don't know what using postgresql-simple looks like:

```haskell
-- here we're using pg-transact, a wrapper around postgres-simple that
-- makes transactions easier to work with: `DBT m` represents part of
-- a transaction: composite `DBT m` values are guaranteed to be run
-- inside a single transaction. This gives explicit control about
-- transaction semantics.
getBlogPostById :: MonadIO m
                => BlogPostId -> DBT m (Maybe BlogPost)
getBlogPostById postid =
  let q = "select blogpost_id, author_id, title, content from blogposts where blogpost_id = ?"
  in listToMaybe <$> query q (Only postid)
```

Basically, you get:

- SQL queries as text
- parameter interpolation
- composable parsing and serializing

### What I always end up using

In Scala, I end up building [anorm-pg-entities](https://github.com/CleverCloud/anorm-pg-entity). When I changed jobs and switched to Haskell full-time, I more or less ported it.

Before talking libraries, the first thing I do when building a DB access layer is to represent the DB schema in code.

#### Introducing types

My first step is to declare one record type per entity table (maybe also association tables, if they have parameters). Pure association tables can very well be represented with tuples. This kind of things is often called a DAO, for _Database Access Object_.

The most important thing here is to make these types _as obvious as possible_:  
plain records, with no external dependencies (unless super stable core types). Even though I mostly use UUIDs as primary keys, I still newtype every primary key type. The key is to have those types be a faithful description of the DB, in an easy-to-check way. So small types, all defined in dedicated modules, with limited external dependencies. As with types modelling JSON structures, these types have to be handled with care, as they turn untyped properties into types.

This might feel like boilerplate, but consider that you're mapping something that exists _outside_ the compiler-guaranteed realm of consistency. In addition to that, here you're not mapping the DB schema exactly, but rather _common projections_.  
Be always extremely wary of solutions that promise to remove boilerplate _across untyped boundaries_, if they're not based on sound principles. In the case of SQL, one such solution would be to derive the actual schema from the types.  
And since we want proper control over the actual schema, the types would need to be an obvious 1:1 description of the schema (unlike what's usually done by ORMs).

Once you have that, you should always strive to talk to the DB through them. They provide a compiler-checked, easy-to-reason-about model.

With generic derivation, you can get parsers and serializers for free. Reducing boilerplate is a great way to reduce serialization issues. It's not enough though, so I usually test those instances with a roundtrip test (more on that later).

```haskell
newtype BlogPostId = BlogPostId { getBlogPostId :: UUID }
  deriving newtype (Eq, Show, FromField, ToField)
newtype AuthorId = AuthorId { getAuthorId :: UUID }
  deriving newtype (Eq, Show, FromField, ToField)
  
data BlogPost
  = BlogPost
  { blogPostId :: BlogPostId
  , authorId   :: AuthorId
  , title      :: Text
  , content    :: Text
  , createdAt  :: UTCTime
  }
  deriving (Eq, Show)
```

If you choose to have a dedicated model layer, going through these types first will let you type check the model -> DB transformations. Projecting model types directly to DB queries when you don't have a one-to-one mapping can be tricky: you end up having surprising code in parsers and serialization (IMO, you don't want that, as you're already in untyped territory).

Most of the time, my model layer is just aggregated entity types, as I need to carry the ids around anyway: a usual sequence of operations is:

- retrieve data from DB with an id
- act on the retrieved piece of data (model layer)
- persist updated data

In this case, I need to be able to do a DAO -> model transformation, and then a model -> DAO transformation back for persistence. If my model throws away DB-related information, then I have to carry this information around in another channel to be able to go back to DAOs. This usually ends up either compromising type safety, introducing magic in the serialization layer, or giving rise to model types that are isomorphic to DAOs, with extra ceremony.

#### Declaring a mapping

Those types declare what a row looks like in each table. That's not enough: we want to know about table names, field names.

That's the first part of my libs: a typeclass

```haskell
-- This is a simplified version. The real version has optional type annotations
-- for fields, and has a dedicated class for primary keys
-- This requires the `AllowAmbiguousTypes` extension to work: notice that the `e`
-- type does _not_ appear in the method types. It can't be inferred and has to be
-- provided explicitly through visible type applications.
-- Another possibility is to use `Proxy e` as arguments in the methods: it removes
-- the ambiguity (at the cost of increased verbosity).
class Entity e where
  tableName  :: String
  primaryKey :: String
  fields     :: [String]

instance Entity BlogPost where
  tableName  = "blogposts"
  primaryKey = "blogpost_id"
  -- ^ this could be derived automatically from a `Typeable` constraint, but
  -- handling pluralisation automatically is not really something i want to spend
  -- time on
  fields = [ "blogpost_id"
           , "author_id"
           , "title"
           , "content"
           , "created_at"
           ]
```

#### SQL generator goes brrrrrrrrr

Once you have all that, you can automate basic CRUD operations: you have all the information you need to generate basic `INSERT`, `SELECT`, `UPDATE` and `DELETE` queries.

```haskell
-- simplified version of the actual helper, but conceptually it's the same
-- This relies on the `TypeApplications` and `ScopedTypedVariables` extension,
-- to tell the compiler which `Entity` instance we need.
getById :: forall a b m
         . (Entity a, FromRow a, ToField b, MonadIO m)
        => b -> DBT m a
getById pk =
  let q = selectQuery @a <> " where \"" <> primaryKey @a <> "\" = ?"
  in listToMaybe <$> query q (Only pk)

-- you can do that only if all values are generated before talking to the DB.
-- else, you'd need a separate type representing an _entity creation request_.
insertEntity :: forall a m
              . (Entity a, ToRow a, MonadIO m)
             => a -> DBT m ()
insertEntity entity =
  let q = insertQuery @a
  in void $ execute q entity
```

Generating ids and dates ahead of insertion means you don't have to maintain a specific type for insertion. This nice property may go away at some time, but it's quite convenient when starting a project.

This is the right moment to write basic tests to make sure the basics work, using [tmp-postgres](https://hackage.haskell.org/package/tmp-postgres) to spin up a temporary DB for tests.

```haskell
testRoundtrip :: BlogPost -> Assertion
testRoundtrip blogPost = do 
  insertEntity blogPost
  result <- getById (blogPostId blogPost)
  assertEqual result (Just blogPost)
```

At this point you may think I said all that just to showcase a shitty untyped ORM ersatz. I'd argue that's a neat improvement over more naïve uses of postgresql-simple, but let's be honest, simple single-table queries are not why you're using PostgreSQL in the first place.

The interesting part is when you want more complex queries. That's where typed layers and ORMs start getting annoying. SQL is a rather fine language to describe queries, spending time to find the right incantations to have a layer generate the SQL you already have in your head gets tedious quickly.

In my experience, the issue with manually written SQL was when the schema got updated. I had to go over every query and make sure fields were correctly listed. That was the number one cause of regressions in the SQL layer.

The interesting part is the _structure_ of the query: which tables you are joining, and how. Listing fields in the query is tedious, and combining parsers is usually trivial. A nice property of having one type per table is that joins are made obvious in the types. So when you retrieve an object from the DB, you can have a good idea of the query that was sent.

Since we have mapped the field names and types, we can mix static SQL (the aforementionned _structure_ of the query) with a dynamically generated list of fields.

Does this prove your SQL statements are well-formed in 100% of the  
cases? No. However it succeeds at sidestepping many issues with manual SQL queries, while keeping you in control.

Since I introduced this layer at work, my colleagues have started to use and extend it. The base features presented in this post are my work (except generic derivation of `Entity`, which I have only implemented in Scala, while [@ptit\_fred](https://twitter.com/ptit_fred) handled it in Haskell).

## Still, it's not perfect

Even though it has proven effective to avoid common issues, we're still generating strings at runtime. This makes static analysis of queries hard and we have to rely on tests for a lot of things.

Even though the _projection_ part is handled and provides a modicum of safety, we are still completely on our own on `where` clauses.

Finally, the whole mapping makes entity-level operations easy, but for partial updates and partial projections you're on your own. Designing everything around entities instead of actual use cases feels backwards, especially since I've been exposed to Domain Driven Design. Here it's a purely pragmatic choice: a base language allowing to manipulate things at the entity level is surprisingly scalable and allows to handle a lot of use cases as they come.

## Possible improvements

I'm perfectly happy with these helpers used internally, in a rather controlled setting. I don't think they are principled enough to be shared as an open-source library. The core tenet of this internal library is that it should not try to model SQL itself. In extremely simple cases, it is able to produce structured SQL, but at its core its only job is to keep track of field lists. This comes in tension with the natural desire to have it handle more things for us. However it sits on a _very_ narrow sweet spot, where it's powerful enough to be useful, but simple enough to stay out of the way. Other DB access libraries have stronger goals: ranging from staying out of the way completely (postgresql-simple) to completely taking over (persistent).

One library that stands out from the others is [squeal](https://hackage.haskell.org/package/squeal-postgresql), which faithfully embeds SQL semantics in Haskell. We use it at work in a few projects that benefit from fully typed queries. I like that it does not _try_ to hide SQL semantics away. Unfortunately, for now, it does have a cost, in terms of compilation times (it's being worked on in GHC), in terms of readability (that's obviously subjective, but the type-level encoding warrants some extra syntax that makes "seeing" the original SQL rather hard), and in terms of what's possible to automate (manipulating strings is easy, manipulating type-level constructs… not so much). This requires the developer to always be extremely explicit, even for simple queries working with common projections. Note these are not fundamental issues. This lib sits on the other side of the convenience tradeoff and would be my go-to choice for more complex domains. That's talk for another day, but having this layer completely typed (and kept in sync with the actual schema) allows to completely avoid the "simple, stable DAOs" requirement, since everything can now be checked statically (ad-hoc projections, query parameters, …).

This is where I'm at right now, my core choices about SQL are quite stable, but the way I turn that into implementations will surely change (hopefully toward a more typed approach).
