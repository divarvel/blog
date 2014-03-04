---
title: ADT serialization with Play JSON
author: Clement Delafargue
tags: playframework, fp, json
---

I'm currently working on a HTTP API with playframework and part of my data is
modeled using *Algebraic Data Types* (ADTs).

An ADT is roughly a combination of *product types* (multiple values together)
and *sum types* (alternatives).

In scala, a product type can be modeled by a case class and sum types by a
`sealed trait`. The `sealed` keyword ensures the closing of alternatives.

```scala
case class Point(x: Float, y: Float)

abstract sealed trait Shape
case class Circle(origin: Point, radius: Float) extends Shape
case class Polygon(points: List[Point]) extends Shape
```

Play 2.1 provides a great combinator-based API, as well as type-classes
(`Reads` and `Writes`) to go back and forth between scala values and
Json values.

For a quick refresher on typeclasses:
<http://blog.eklaweb.com/2012/05/23/champs-de-formulaire-custom-avec-play-2/#les_typeclass_en_scala> (sorry, it's in French)

Since there is an isomorphism between case classes and a subset of json
(homogeneous lists, and a few more restrictions), Play-JSON is able to derive
`Reads` and `Writes` instances from case classes with some macro-fu
(props to [@mandubian](http://mandubian.com)).

Mandubian helped me refactor my instances to a *pointfree-er* style which
stresses better the combinatorial nature of the JSON API.

```scala
implicit val pointReads = Json.reads[Point]
implicit val pointWrites = Json.writes[Point]
```

However sum types are not supported by this automatic derivation, so you have
to do it by hand:

```scala
import play.api.libs.json._
import play.api.libs.functional.syntax._

implicit val shapeReads = {
  val cr = Json.reads[Circle]
  val pr = Json.reads[Polygon]
  __.read[Circle](cr).map(x => x: Shape) |
  __.read[Polygon](pr).map(x => x: Shape)
}

implicit val shapeWrites = Writes[Shape] { shape => shape match {
 case circle: Circle => Json.writes[Circle].writes(circle)
 case poly: Polygon =>  Json.writes[Polygon].writes(poly)
} }
```

Observe that we don't need to create implicit `Reads` and `Writes` instances
for `Polygon` and `Circle`, since everything will be handled by `shapeReads`
and `shapeWrites`. We directly use the generated instances inside `reads` and
`writes`. We could have exposed these instances to the outside world but
they're not needed elsewhere and it would create overlapping instances.

Enjoy :)
