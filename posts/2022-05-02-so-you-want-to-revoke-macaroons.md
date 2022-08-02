---
title: So you want to revoke macaroons
author: Clement Delafargue
tags: haskell
canonical: https://tech.fretlink.com/so-you-want-to-revoke-macaroons/
---

Macaroons are bearer tokens. As for every bearer token, the question of revocation is important: how can we stop a token from being accepted by services?

![Danny Glover as Roger Murtaugh in Lethal Weapon 3](/files/its-just-been-revoked.jpg)

Macaroons are bearer tokens, introduced in [Macaroons: Cookies with Contextual Caveats for Decentralized Authorization in the Cloud](https://research.google/pubs/pub41892.pdf). There is a [reference implementation](https://github.com/rescrv/libmacaroons) that pins down the details.

As for every bearer token, the question of revocation is important: how can we stop a token from being accepted by services? The usual challenge with revocation is to distribute the list of revoked tokens to every service. In the case of macaroons, there is an extra challenge: uniquely identifying a token. This is what I’ll talk about in this article. Distributing revocation lists is a really interesting subject, but is highly context-dependent, and abundantly covered elsewhere.

Here is what we’ll cover:

- first a recap about bearer tokens and their tradeoffs;
- then a description of macaroons, with details relevant to revocation;
- then we will cover two possible solutions;
- finally some advice about what can be done, and what should be done now.

## The tradeoff of bearer tokens

Bearer tokens don’t need shared state: they are supposed to be verifiable without accessing a central authority. It is usually done through the use of cryptographic tools, such as [HMAC](https://en.wikipedia.org/wiki/HMAC), or [digital signatures](https://en.wikipedia.org/wiki/Digital_signature).

The benefit of not having shared state is that each service can independently trust a bearer token without performing extra network requests. This removes a single point of failure and makes decentralized systems easier, but the drawback is…  
_not having shared state_.

With session tokens, everything is immediate, because the session is the single source of truth. Bearer tokens trade immediate updates for simpler semantics and more robustness.

In particular, revoking access is simple with session tokens: deleting the session automatically revokes all linked tokens, with no latency.

### Revocation lists problems

Revoking a bearer token is harder: since each service can verify them independently, you need to push down a list of revoked tokens to each service. This means that revoking access can take much longer, depending on the precise mechanism used to distribute the revocation list.

#### Latency

Of course, reducing revocation latency is extremely important. If there’s a leak, we want to plug it as fast as possible, reducing the vulnerability window as much as possible. In trying to do so, we still must be careful about not negating the benefits of bearer tokens. For instance, it might be tempting to contact a revocation server every time a token is verified. That reduces latency to zero, but this sneakily reintroduces a network dependency and a single point of failure, with much more complexity.

#### List size

Once a token is revoked, you need to reject it until the heat death of the universe (or more realistically until your rewrite everything to use shiny new tech). This means that the revocation list can only grow. Not an issue at first but it can become an issue after a couple years (especially if you resist the urge to rewrite everything with shiny new tech).

One common way to mitigate this is to add TTLs to all your tokens, this way you can remove them from the revocation list once they are expired.

### Revoking macaroons

In the case of macaroons, we have an extra challenge: how do we uniquely identify a macaroon? Just listing the macaroon is both too broad and not broad enough, as you will see.

## What is a macaroon?

The main benefit of macaroons is that they allow _offline attenuation_: given a token, its holder can craft a new token, with more restrictions, _without_ interacting with the token emitter.

### Seriously, what _is_ a macaroon?

A macaroon is a token made of several parts:

- a _key identifier_, that provides the base scope for the token
- a _location,_ a freeform field that is not cryptographically signed and is purely informational. We will ignore it from now on.
- a series of _caveats_, restrictions on this base scope
- a _signature_ that proves the macaroon comes from a trusted authority

The _key identifier_ and the _caveats_ are freeform bytestrings. They usually contain text, but they can contain anything.

Usually it is enough to know that the _key identifier_ is the only part that you can trust as coming from the authority and that the caveats can be freely added to a given token, but not removed. In our case, we will have to dig a bit more into the cryptography part to understand how to identify a macaroon.

Verifying a macaroon involves two operations:

- verifying the signature (more about this later);
- making sure each _caveat_ is _discharged_.

Let’s clarify what _discharging a caveat_ means and how it’s done in practice. A _caveat_ is a restriction carried by the token: the token is only valid if the verifier can prove the property it ensures is satisfied. A common caveat is `time < 2022-04-30T00:00:00Z`. It means "this token is only valid as long as the current datetime is before April 30, 2022, midnight UTC. For a macaroon to be valid, all of its _caveats_ must be discharged. To discharge _caveats_, the verifying party can supply _verifiers_. A _verifier_ is a function taking a _caveat_ (a bytestring) and returning a boolean (`true` if the verifier understands the _caveat_ and can discharge it, `false` if it cannot understand it, or if it understands it and can’t discharge it). Here is how it looks like in haskell:

```haskell
timeVerifier :: UTCTime -> ByteString -> Bool
timeVerifier now = fromMaybe False $ do
  dateTimeStr <- BS.toString <$> BS.stripPrefix "time < "
  validBefore <- Time.iso8601ParseM dateTimeStr
  pure $ now < validBefore
  
verifyMacaroon :: Macaroon -> IO Bool
verifyMacaroon macaroon = do
  now <- Time.getCurrentTime
  pure $ verify secret macaroon [timeVerifier now]
```

Here, `verify` performs both verifications: it checks the signature, and it tries to discharge every _caveat_.

### Third-party caveats

The _caveats_ I have described above are called _first-party_ caveats. It means they can be discharged directly by the service who verifies the macaroon. Macaroons also describe another kind of _caveat_ (and it may be their most important innovation), called _third-party_ caveat. It is a _caveat_ that can only be discharged by a third-party service. In practice, it means that the macaroon holder has to contact the third-party service to get a _proof_ that the caveat can be discharged, in the form of (you guessed it) _another macaroon_ (which, crucially, can contain _caveats_ of its own). The client has then to send both macaroons (serializing multiple macaroons is not spec'd in the paper, nor provided by the [reference implementation](https://github.com/rescrv/libmacaroons)) to the service that will then be able to discharge the third-party _caveats_ with the attached _discharge_ macaroons.

This is a very powerful mechanism, as it allows verification to be split across multiple services, _without requiring the services to talk to each other_. The only requirement is a shared secret between services. This is very useful when coordinating services across multiple independent entities (for example restricting access of a Dropbox link to Facebook friends).  
Within services of a single entity, however, it tends to be overkill, as strong de-coupling of services is not as useful. Sadly, I have heard of a couple macaroons deployments going this way (because it is tempting, fun and interesting), and then suffering from complexity. Interestingly enough, I have not heard of cross-company macaroons deployments.

### Macaroon signatures

Now, for the signature part. Macaroons are based on HMACs: this allows to sign a payload with a shared secret: the verifying party computes the expected result and checks if it is the same as the provided signature.

To sign a macaroon, the emitter proceeds as follows:

* first it takes the HMAC of (something computed from) the key identifier, signed by the macaroon secret;
* for each caveat, it takes the HMAC of the caveat, signed by the previous signature
* it bundles the _identifier_, the _caveats_ and the final signature together

With this, you should be able to convince yourself that holding a token allows you to add a _caveat_ without knowing the shared secret, and that it’s impossible to remove or reorder _caveats_ without knowing the shared secret.

To verify a macaroon, the verifying party computes the expected signature and checks if it matches the one carried by the macaroon.

## Part two: how to identify a macaroon

Okay, so now your services maintain a revocation list. Good. What are you going to put in it?

You could store the whole macaroon but that is not super convenient, and you would need to be careful with the revocation list, since macaroons are sensitive tokens after all.

### Using a third-party caveat tied to an external revocation service

The macaroons paper does not cover revocation explicitly, but hints at it in an example of how third-party _caveats_ could be used: when created, a macaroon can be attenuated with a third-party _caveat_ that mandates a proof that the macaroon has not been revoked. In practice, it means that the client has to call out to a revocation server, to obtain a proof that the current token has not been revoked (how?), and bundle this proof with the original macaroon before finally sending the request to the service.  
For. Every. Single. Request.

That sounds… cumbersome. Of course the client can cache validity proofs, but that still makes the client code way more cumbersome. The discharging of the third-party caveat can also be shifted to the service, but at that point, why use a third-party caveat at all? The service can just call the revocation server itself.

All in all, while this way of doing things is explicitly advised in the paper, I really don’t think it’s a convenient way to do things. Worse, the complexity it brings might lead you to doing unfortunate things like calling the revocation service at every request (thus negating all benefits of using bearer tokens), without really realizing it.

### Using the _key identifier_

You could reject all macaroons with a given _key identifier_. In some cases it is a practical solution: if the _key identifier_ is a session id, you end up in the session token case: you nuke the session and force a re-auth.

Some macaroon deployments will work well with this setup, but for pure bearer macaroons it will be too broad: if the _key identifier_ only identifies a user, then the user would be banned forever. Not satisfying.

### Using the signature

Another solution would be to store the macaroon signature: it’s computed from the key id and the caveats, so it allows you to identify a given macaroon.

All good then? Sadly, no. Such a system would be trivial to circumvent.

Macaroons are made to be restricted with _caveats_. So given a macaroon, you can mint a new one with an arbitrary _caveat_ (say, a TTL caveat that expires in a thousand years), and get a new macaroon, with a new signature.

Thankfully, checking signatures can still be useful: the signature carried by a macaroon can be changed by adding a _caveat_, but during the process of verifying a macaroon signature, you compute all intermediate signatures. So it is possible to check each intermediate signature against a revocation list. This solution is great because it means that if you mint a restricted macaroon from a base macaroon, and want to revoke only this restricted macaroon, you can do it without revoking the base one. Conversely, if you revoke a macaroon, all of its derived macaroons are automatically revoked.

This sounds like a great solution, but there are two issues with it:

- macaroon libraries don't expose intermediate signatures
- you're still not able to _uniquely_ identify macaroons

Macaroon generation is deterministic: there is no randomness involved. With the same key id and caveats, you'll get the same signature. So when you revoke a macaroon based on its signature (or any intermediate signature), you don't revoke just this macaroon, but all macaroons generated with the same key id and caveats.

#### With TTLs

Now, if you generate macaroons with TTLs, you get closer to unicity: revoking a macaroon will only revoke macaroons with the same key id, caveats (and this means that have been generated at the exact same moment, depending on the precision of the timestamp carried by the TTL caveat).

It is still not completely satisfying, since it is rather accidental, absolutely not random, and because some macaroons may not have TTLs (API-to-API macaroons for instance).

### With a dedicated id

The best solution to make a macaroon unique is to include a random id upon generation. A simple way to do it is to add a dedicated caveat. Once you have that, every macaroon is unique when generated. You can do the same for derivation: if you want to be able to uniquely revoke a derived macaroon, you need to include a new unique id along with the caveats you're adding.

```
not_revoked = <random_blob>
```

But now, if you're adding unique ids in caveats, you don't need access to intermediate signatures any more: the verifier for these caveats can check if they are not revoked. This is coarser grained than checking signatures (you can only target ids, not single out caveats, and if you have a macaroon with no id, then you can't do anything), but that can be easily implemented without needing any library change.

Here is how it could look like in haskell:

```haskell
revocationValidator :: (ByteString -> RevocationResult)
                    -> ByteString
                    -> Bool
revocationValidator isRevoked caveat =
  let rid = BS.stripPrefix "not_revoked = " caveat
   in case isRevoked <$> rid of
        Nothing    -> False
        -- ^ this is not a revocation caveat
        Just True  -> False
        -- ^ this id has been revoked, the caveat is not discharged
        Just False -> True
        -- ^ this id has not been revoked, the caveat can be discharged
```

Just remember that, due to how macaroons work, if you don't have a revocation caveat, then the revocation check will not run. Embedding a revocation id in newly created macaroons is not enough, you’ll need to add a revocation check to all existing long-lived macaroons, and, after some time start manually checking for the presence of revocation ids as part of the verification pipeline.

```haskell
hasRevocationId :: Macaroon -> Bool
hasRevocationId macaroon =
  let isRevocationCaveat bs = BS.startsWith bs "not_revoked = "
   in any isRevocationCaveat $ caveats macaroon

verifyMacaroon :: (ByteString -> RevocationResult)
               -> [ByteString -> Bool]
               -> Macaroon
               -> IO Bool
verifyMacaroon isRevoked verifiers macaroon = do
  now <- Time.getCurrentTime
  let allVerifiers = timeVerifier now
                   : revocationVerifier isRevoked
                   : verifiers
  pure $ hasRevocationId macaroon
      && verify secret macaroon allVerifiers
```

Having to embed this kind of manual check to macaroons validation is one of the reasons why you _really_ should build an auth layer on top of macaroons and not just use them directly.

## What you should do now

The first question you should ask yourself is "why don't I just use session tokens?". Using session tokens instead of bearer tokens makes _a lot_ of things easier.

But if you're set on bearer tokens, particularly macaroons, then read on.

### Use TTLs

Make sure you use TTLs. For tokens automatically delivered, bake it in the delivery process. Short-lived tokens can be coupled with a refresh token update mechanism. When using long-lived tokens, adding a TTL right before sending them on the wire mitigates the consequences of a leak.

TTLs won't make revocation go away, but it will definitely make it easier and restrict the scope of security issues.

### Embed unique ids in macaroons

Even if you don't end up implementing revocation capabilities, being able to identify tokens will help your ops team at some point. Once you've done that, not only you will be able to trace the use of tokens and know where they come from, but you'll also be able to roll out revocation in the future without having to renew every token.

### Ensure that tokens are revocable

Adding revocation ids to new tokens is not enough. For short-lived tokens, you can wait a bit and now every token will have a revocation id, but for long-lived tokens you will have to go and replace them with fresh tokens with revocation ids.

The next step will be to make sure verified tokens have revocation ids. You can start by logging a warning when you see an unrevocable token. After a while, when logs have become quiet, you can flip the switch and start refusing unrevocable tokens altogether.

## Think about how to distribute revocation lists

Start simple. If you don't have a lot of services, a static revocation list read at startup (from a file, or an environment variable) can be more than enough. Then you can think about out-of-band polling, or even push notifications. Try to keep things simple, you don't want too many moving parts. Just keep in mind that if you end up calling a central revocation service every time you're checking a token, you might be better off with a centralized session.

## But seriously, consider using [biscuit](https://biscuitsec.org)

[Biscuits](https://biscuitsec.org) have been designed with macaroons
strengths (and weaknesses) in mind, so they tend to provide out of
the box all the mitigations required by macaroons. For instance they
provide unique revocation ids as part of the spec and as such as part
of the core API.
