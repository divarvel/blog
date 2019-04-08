---
title: TypeSafe URLs in Lift
author: Clément Delafargue
tags: archives
---

*This article is part of a series of articles I have extracted from my
previous blog. Most of these articles are out of date, and some of them are
terribly misguided.*

I've been working for a few months with [Lift](http://liftweb.net/), a
really nice web framework. Lift takes ideas and concepts from a few
other frameworks. The main difference between Lift and other frameworks
is that Lift does not enforce MVC (even though you can do MVC if you
want to). Lift's approach is **view-first**, which means you first
create the view, then you tell which part of the code will take care of
dynamism.

For more propaganda: [Seven Things](http://seventhings.liftweb.net/) and
[Web Framework
Manifesto](http://lift.la/web-framework-manifesto-republished-from-2006)

I'll explain how to build *REST-y* URLs in a type-safe manner, with the
SiteMap. The goal is to access a blog post with this kind of URL:
**/posts/*{id}***

I'll illustrate this article with something tremendously original: a
Blog, with posts. I'll use mongoDB as a backend, but it's not really
important.

### The model

It's a simple blog post. Let's just assume the author is stored in
plain text.

```scala
class BlogPost private() extends MongoRecord with ObjectIdPk[BlogPost]
{
 def meta = BlogPost
 object author extends StringField(this, 32)
 object date extends DateField(this)
 object content extends TextareaField(this, 500)

 val url = ... // We'll see that later
}

object BlogPost extends BlogPost with MongoMetaRecord[BlogPost] {
 val entry = ... // We'll see that later
 def calcHref(bp: BlogPost) = ... // We'll see that later
}
```

### The template

Nothing special there. Just save it in `webapp/posts.html`. We have a
`h1` for the title, a `p` for some info, and a `p` for the actual post.

```html
<div class='lift:surround?with=default;at=content'>
<div class='lift:BlogPostSnippet'>
<h1 class='title'></h1>
<p class='info'></p>
<p class='blogpost'></p>
</div>
</div>
```

### The snippet

This class will be instantiated with the blog post we want to display.
We just inject data in the markup.

```scala
class BlogPostSnippet(val blogPost: BlogPost) {
 def render = '.title *' \#> blogPost.title.is &
 '.info *' \#> ('By ' + blogPost.author.is) &
 '.blogpost *' \#> blogPost.content.is
}
```

### The SiteMap

So, back to `BlogPost.entry`, where the magic happens. We define an
entry of the sitemap which is associated with the `BlogPost` class. The
two first arguments are the id and the displayed name of the link. The
third argument is a function which takes a `String` and returns a
`Box[BlogPost]`. This tells Lift how to instantiate `BlogPostSnippet`
with the right blog post. The fourth argument is used to create an url
from a given blog post. Finally, we specify the address and tell Lift we
don't want this entry to show up in the menus.

The calcHref method is just a shortcut.

```scala
class BlogPost ... {
 /* Snip */
 val url = BlogPost.calcHref(this)
}

object BlogPost ... {
 val entry = Menu.param[BlogPost](
 'blogpost',
  S ? 'blogpost.view',
 (id: String) => BlogPost.find(id),
 (bp: BlogPost) => bp.id.is
 ) / 'posts' / * >> Hidden

 def calcHref(bp: BlogPost) = entry.toLoc.calcHref(bp)
}
```

Don't forget to add `BlogPost.entry` to the `SiteMap`

```scala
val entries = /* other entries */ :: BlogPost.entry :: Nil
LiftRules.setSiteMap(SiteMap(entries: _*))
```


Next time I'll show you how to display comments after the blog post,
while keeping the HTML structure of every comment in the template :)
