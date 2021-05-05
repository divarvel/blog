---
title: IFTTD, la suite
author: Clement Delafargue
tags: haskell
---

J'ai eu la chance de participer à IFTTD pour parler de haskell (<https://ifttd.io/de-limportance-dun-typage-qui-fonctionne/>). C’était très sympa mais évidemment dès la fin du podcast je me suis aperçu que je n’avais pas mentionné tout ce que je voulais.
Je vous mets tout ça pêle-mêle ici, si l’épisode vous a intéressé, peut-être que ces quelques notes feront un bon complément. Si vous avez des questions, n’hésitez pas à m’en parler sur [twitter](https://twitter.com/clementd) ou sur [mastodon](https://framapiaf.org/@clementd), je serai ravi d’y répondre.

# les ide

Ça a longtemps été un point noir (car on s’en passe très bien, contrairement à java), mais ça reste sympa. Regardez hls dans VSCode, ça marche bien et ça s’améliore constamment.

Si vous voulez un truc simple qui marche, regardez du côté de `ghcid`, qui lance la compil en boucle
et qui est très rapide.

# hlint

Je n’ai même pas pensé à le mentionner, mais c’est un outil fondamental pour l’apprentissage de haskell.
Oubliez les linters JS qui se plaignent sur le formattage du code. Là c’est des suggestions utiles,
et qui vous apprendront des trucs sur haskell.

**hlint est un formidable outil pédagogique**.

Si vous utilisez `ghcid`, lancez `ghcid -l` pour lancer `hlint` après la compilation.

# les type classes et l'abstraction

Fun fact, le podcast a été enregistré deux fois, car le premier enregistrement a été perdu à cause d’un problème technique. C’est dommage, car on y a eu une discussion très intéressante sur l’abstraction et l’indirection.

Au delà de l’évaluation paresseuse, l’autre innovation de haskell, ce sont les type classes. C’est un mécanisme de modularité qui a depuis été repris d’une manière ou d’une autre par scala, rust, et même go avec go generics.

En gros, une typeclass c’est un peu comme une _interface_ en java (ça définit des méthodes, certaines à implémenter, d’autres déjà implémentées en fonction des autres), mais ça diffère de java en deux points fondamentaux :

- en java, un type doit implémenter une interface à sa déclaration. En haskell, on peut implémenter de nouvelles typeclasses pour des types existants.
- en java, on utilise des interfaces principalement via le sous-typage. En haskell, on utilise les typeclasses comme contrainte sur un type générique

Pour la première propriété, ça évite d’avoir à tout wrapper, et ça permet aussi de s’intégrer proprement dans une lib ou un framework web par exemple, sans avoir une séparation nette entre "c’est prévu par le framework" et "c’est pas prévu par le framework".

Pour la deuxième propriété, c’est un peu plus subtil. Quand on passe par du sous-typage, on utilise le supertype _à la place_ du type réel. Du coup si on a deux types qui implémentent la même interface,
on se retrouve un peu bloqués. Il y a des solutions pour utiliser le sous-typage, tout en conservant
le type d’origine, mais c’est un peu alambiqué (cf f-bounded polymorphism). C’est parfois utilisé en java (cf `Comparable<T>`) mais souvent on va plutôt se retrouver avec du `Object` pour ne pas s’embêter.

Étant donné que l’OO à la java nous a pourri le cerveau en mélangeant héritage et sous-typage, et qu’en plus, les abstractions introduites sont souvent pas très bien conçues, "abstraction" ça fait un peu peur. Mais c’est souvent parcequ’on confond abstraction avec indirection.

L’indirection, c’est mettre des couches intermédiaires au dessus de notre code pour le rendre plus flexible. On s’éloigne du modèle concret, mais sans gagner en clarté.

L’abstraction est là pour exprimer avec clarté des concepts transverses, en "oubliant" les détails non nécessaires. Quand c’est bien appliqué, c’est une force _simplificatrice_ : en utilisant des abstractions dans le code, on contraint les comportements à suivre des propriétés précises.

Une bonne abstraction, c'est un compromis entre deux propriétés : son applicabilité, et sa puissance.
En haskell, l’applicabilité, c'est le nombre de types qui implémentent une typeclass donnée. Par exemple `Functor` est très applicable. Sa puissance, c’est ce qu’on peut faire avec. En haskell, c’est les types des méthodes de la classe. Par exemple, la classe `Monad` permet de faire plus de
choses que `Functor` (par exemple chaîner des opérations), mais est implémentée par moins de types.

On peut voir l’abstraction comme gênante, quand elle nous force à oublier les types concrets. D’une part on peut bénéficier de l’abstraction sans forcément oublier le concret, et d’autre part, ce n’est pas forcément une mauvaise chose.

On peut se servir localement d’une abstraction, sans oublier les types : par exemple quand je fais
`("Toto" <> "Toto") :: Text`, j’utilise une abstraction (la notion de _semigroup_ (_demi groupe_ en bon français), tout en conservant des valeurs de type `Text`. Je n’ai rien perdu sur le plan concret, mais j’ai un code qui m’indique clairement (pour peu que je connaisse les propriétés d’un _semigroup_) les propriétés qu'il respecte.

On peut aussi se forcer à n’utiliser que ce qui est nécessaire, en utilisant des types paramétrés et en ne listant que les typeclasses dont on a besoin. Là on perd le côté concret, mais on y gagne beaucoup en échange : on a la garantie _en lisant les types_ que le code va se comporter uniquement
en respectant les propriétés des typeclasses concernées.

Par exemple, `Monoid a => [a] -> a` me dit que les éléments de la liste d’entrée vont être combinés, uniquement suivant la structure de la liste, sans faire de comparaisons entre éléments (pas de dédoublonnage par exemple), ni d’opération sur les éléments (si j’appelle cette fonction sur une liste de `Text`, je sais par exemple que la casse ne va pas être modifiée, que des espaces ne vont pas être intercalées, …). Si je vois `[Text] -> Text`, je n’ai aucune idée de ce qui peut se passer en pratique.

Ces deux approches sont complémentaires, parfois on a un contexte bien précis et c’est plus intéressant de réfléchir en termes de types concrets, parfois c’est plus pertinent de réfléchir en termes de propriétés plus génériques. Et on peut mélanger les approches : par exemple `Monoid a => [a] -> a`,
c’est un mélange entre un type concret (`[]`) et un type paramétré (`a`). J’aurais pu aussi avoir
`Monoid a, Foldable t => t a -> a`, qui restreint encore un peu les possibles.

<https://www.youtube.com/watch?v=GqmsQeSzMdw>

Encore une fois, haskell nous permet de choisir, ici. Là c’est des exemples simples, mais c’est un choix classique d’architecture des programmes en haskell, pour la représentation des effets, on peut soit choisir des types explicites (`IO`, ou `ReaderT Env IO`), soit utiliser des types contraints par les opérations que l’on veut réaliser (`MonadReader Env m => …`), tout en pouvant mélanger les deux (`MonadReader Env m => ExceptT Error m …`) dans certains cas. On a des outils à notre disposition, après c’est la responsabilité de la personne qui écrit le code de faire des choix
éditoriaux sur les propriétés intéressantes à mettre en avant.

# ressources

Pour les débutant·e·s et intermédiaires (mais même des plus expérimenté·e·s peuvent y apprendre des trucs).

- <https://typeclasses.com/>

Deux bouquins pour mettre en pratique

- <https://leanpub.com/finding-success-in-haskell>
- <https://leanpub.com/sockets-and-pipes>

Pour améliorer son style (à suivre au début, pour éviter de se poser trop de questions)

- <https://kowainik.github.io/posts/2019-02-06-style-guide>

Super utile, plein de patterns très répandus mais pas forcément toujours documentés. Là c’est concentré en un seul endroit et bien illustré.

- <https://kowainik.github.io/posts/haskell-mini-patterns>

Une fois que vous avez les bases, je recommande chaudement d’utiliser `relude` qui améliore énormément
l’expérience de dev.

- <https://kowainik.github.io/projects/relude>

Une ressource plus complète qui couvre beaucoup de terrain. Un bon départ pour découvrir un sujet précis
(c’est bien à jour)

- <http://dev.stephendiehl.com/hask/>
