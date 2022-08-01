                   <p>Ah, the <code>do</code>-notation. Is it good, is it <a href="https://mail.haskell.org/pipermail/haskell-cafe/2007-August/030178.html">bad</a>; who knows? It's good for beginners, it's bad for beginners. It's <a href="https://wiki.haskell.org/Do_notation_considered_harmful">considered harmful</a>.</p><p>A lot of <a href="https://joyofhaskell.com/posts/2017-05-07-do-notation.html">virtual ink</a> has been spilled on this subject (<a href="https://twitter.com/search?q=https%3A%2F%2Ftwitter.com%2FFunctorFact%2Fstatus%2F1255566213092184070&amp;src=typed_query">and still is</a>). Let me try to add a new perspective, informed by the use of Haskell in a heterogeneous team, in a professional setting. Then we'll be able to gleefully conclude that <em>it depends</em>. But rather than fretting too much over the destination, let's try to enjoy the journey.</p><h2 id="what-s-in-a-do">What's in a <code>do</code>?</h2><p>Let's start by the beginning. <code>do</code> blocks are a syntactic construct in the language Haskell. It provides an alternative syntax over the use of <code>&gt;&gt;=</code>:</p><pre><code class="language-haskell">-- without do
main :: IO ()
main =
  getLine &gt;&gt;= \name -&gt;
  let greeting = "Hello " &lt;&gt; name
  in putStrLn greeting

-- with do
main :: IO ()
main = do
  name &lt;- getLine
  let greeting = "Hello " &lt;&gt; name
  putStrLn greeting</code></pre><p>That's the gist of <code>do</code>-notation. There are a few additional features, but we don't need to talk about them for now. The example is formatted in a way that makes the correspondence obvious. You might be tempted to think that <code>do</code> is a smart trick to remove syntactic noise when writing monadic expression. And you'd be right.</p><h2 id="ten-things-i-hate-about-do">Ten things I hate about <code>do</code></h2><figure class="kg-card kg-image-card kg-card-hascaption"><img src="https://s3-eu-west-1.amazonaws.com/tech-blog-fretlink/2020/04/10-things-1.gif" class="kg-image" alt loading="lazy"><figcaption>Julia Stiles as Kat Stratford, saying "But mostly, I hate the way I don't hate you, not even close" in Ten Things I Hate About You</figcaption></figure><p>If you look at things this way, it's easy to discard <code>do</code> notation: it doesn't add any expressive power, and it makes things more complex for no good reason. As Paul Hudack nicely put it,</p><blockquote>Functions are in my comfort zone; syntax that hides them takes me out of my comfort zone.</blockquote><p>Another issue is that it makes imperative code <em>too</em> nice-looking, thus earning Haskell its famous <em>finest imperative language</em> badge of shame. The familiarity provided by <code>do</code> notation might encourage imperative-minded programmers into putting too many things in <code>IO</code> (or any other monadic context, for that matter) instead of <em>composing functions</em> as <code>${DEITY:-or lack thereof}</code> intended.</p><h2 id="love-me-do">Love me <code>do</code></h2><figure class="kg-card kg-image-card kg-card-hascaption"><img src="https://s3-eu-west-1.amazonaws.com/tech-blog-fretlink/2020/05/abbey-road.jpg" class="kg-image" alt loading="lazy"><figcaption>The Beatles, crossing the street from right to left</figcaption></figure><p>All that is good and well, but let's go back to our nice (and artificial) example:</p><pre><code class="language-haskell">main :: IO ()
main =
  getLine &gt;&gt;= \name -&gt;
  let greeting = "Hello " &lt;&gt; name
  in putStrLn greeting</code></pre><p>What do you think happens when you insist on using function composition in a team of Haskell programmers?</p><p>Yeah, that's right, <code>getLine &gt;&gt;= putStrLn . ("Hello " &lt;&gt; )</code>. And <em>in real life</em>, stuff like <code>(runService =&lt;&lt; parseConfig) *&gt; otherTask</code> will pop up, because <em>composing functions is good, obviously</em>. I do <em>love</em> the smell of η-reduction in the morning, but hear me out: what if (bear with me) η-reduction was not always good? Sometimes you care about the pipeline of functions, sometimes you care about the intermediate values.</p><h2 id="why-not-both">Why not both?</h2><p>Now for my main thesis: using <code>do</code> over <code>bind</code> (<code>=&lt;&lt;</code> or <code>&gt;&gt;=</code>) is <em>contextual</em>. And the context can be <em>very small</em>. Ask yourself: what part of your program is important enough to be obvious when you read it? Are all those intermediate variables bringing meaning?</p><pre><code class="language-haskell">
parseString :: String -&gt; Handler Int
parseString = …

getString :: Handler String
getString = …

myDoHandler :: Handler Response
myDoHandler = do
  rawString &lt;- getString
  value &lt;- parseString rawString
  log $ "Got value " &lt;&gt; show value
  pure $ mkResponse value

myMixedHandler :: Handler Response
myMixedHandler = do
  value &lt;- parseString =&lt;&lt; getString
  log $ "Got value " &lt;&gt; show value
  pure $ mkResponse value
  
myPointfreeHandler :: Handler Response
myPointfreeHandler =
 let logAnd = log . ("Got value" &lt;&gt;) . show &gt;&gt;= ($&gt;)
 in mkResponse &lt;$&gt; getString &gt;&gt;= parseString &gt;&gt;= logAnd</code></pre><p>Which one do you prefer? My favourite is <code>myMixedHandler</code>: I care about <code>value</code>, so that makes sense to give it a name. On the other hand, the intermediate <code>String</code> value is not that interesting, so I'm okay with hiding it.</p><p>If your favourite is <code>myPointfreeHandler</code>, I don't know what to say. Maybe you won't agree with my conclusions.</p><p>In my opinion, <code>do</code> and <code>bind</code> are <em>complementary</em>: <code>do</code> nudges me into naming variables by providing a clean syntax. <code>bind</code> makes function pipelines more obvious by hiding intermediary values. Both have their uses, even on the same line!</p><h2 id="but-do-discourages-applicative-style-">But <code>do</code> discourages applicative style!</h2><p>That's right, <code>do</code> works with monads (since it desugars to <code>&gt;&gt;=</code>), so that encourages a monadic style where applicative functors could be enough. Two things:</p><ul><li>If you work with a type where applicative and monadic behaviour are inconsistent, you're in for nasty surprises and <code>do</code> blocks are not your most pressing issue</li><li>Choosing between putting emphasis on values or functions is not inherently monadic</li></ul><pre><code class="language-haskell">myParser :: Parser (Int, Maybe String)
myParser = do
  char '@'
  intValue &lt;- int
  many space
  stringValue &lt;- optional string
  pure (intValue, stringValue)
  
myApplicativeParser :: Parser (Int, Maybe String)
myApplicativeParser =
  liftA2 (,)
    (char '@' *&gt; int &lt;* many space)
    (optional string)</code></pre><p>Both parsers parse the same thing, but the second is <em>obviously better</em> because it truly showcases the applicative nature of what we're doing.</p><p>Here, parser has both a <code>Monad</code> and an <code>Applicative</code> instance, so that's just style.</p><p>What about a parser that is <em>not</em> a <code>Monad</code>?</p><p>Let's put our <a href="https://tech.fretlink.com/environment-variables-parsing-for-free-applicatives/">environment variables parsers</a> to work.</p><pre><code class="language-haskell">data MyConfig
  = MyConfig
  { value2 :: String
  , value1 :: String
  , value3 :: Int
  }
  
configParser :: EnvParser MyConfig
configParser = MyConfig
  &lt;$&gt; required (str "VALUE_1")
  &lt;*&gt; required (str "VALUE_2")
  &lt;*&gt; required (int "VALUE_3")
</code></pre><p>Ah that's where applicative syntax shines! It's clear and easy to read. Oh by the way did you spot the <em>surprising behaviour</em>? Yes? Congrats then. Something really close happened to us, got past code review, went to production and caused an outage. Yes we were using newtypes instead of plain strings. No it did not help us (<code>IsString</code> can be tricky). If you did not notice the issue, <code>VALUE_1</code> maps to <code>value2</code>, and <code>VALUE_2</code> maps to <code>value1</code>.</p><p>The issue here is that we rely on the order of arguments in the constructor. A nice rule of thumb when constructing records is to make fields explicit:</p><pre><code class="language-haskell">configParser :: EnvParser MyConfig
configPaser =
  let mkConfig value1 value2 value3 =
    MyConfig { value1 = value1
             , value2 = value2
             , value3 = value3
             }
   in mkConfig &lt;$&gt; required (str "VALUE_1")
               &lt;*&gt; required (str "VALUE_2")
               &lt;*&gt; required (int "VALUE_3")</code></pre><p>That's a bit better, because everything is in the same function: we don't have to look at the record definition to know what will happen. Still, the names are one level away from the parsers.</p><p>It turns out that we can have <code>do</code> notation even without <code>Monad</code>, thanks to <code>ApplicativeDo</code> (I've also thrown a <code>RecordWildCards</code> in the mix since we're playing with <em>forbidden</em> extensions.</p><pre><code class="language-haskell">{-# LANGUAGE ApplicativeDo   #-}
{-# LaNgUaGe RecordWildCards #-}

configParser :: EnvParser Config
configParser = do
  value1 &lt;- required (str "VALUE_1")
  value2 &lt;- required (str "VALUE_2")
  value3 &lt;- required (int "VALUE_3")
  pure Config{..} </code></pre><p>Is it theoretically pure and syntactically minimal? No. Does it provide easier to read code, even in messy, real-worldy context? I do think so, yes.</p><h3 id="applicativedo-pitfalls"><code>ApplicativeDo</code> pitfalls</h3><p>So like me, you think <code>ApplicativeDo</code> is criminally underused? Good! Now might be a good time to mention two things:</p><p><code>ApplicativeDo</code> changes the behaviour of all <code>do</code> blocks. So if you are using types with inconsistent <code>Applicative</code> and <code>Monad</code> instances, you may curse a bit. Purescript solved it nicely by introducing <code>ado</code>, to make the behaviour explicit. Personally, I try to only enable <code>ApplicativeDo</code> in small modules where the only <code>do</code> blocks are over types with no <code>Monad</code> instance.</p><p>Another pitfall is a somewhat surprising behaviour with <code>let</code> bindings in <code>do</code> blocks:</p><pre><code class="language-haskell">-- this requires a `Monad` instance on `EnvParser`
myParser :: EnvParser Result
myParser = do
  v1 &lt;- v1Parser
  v2 &lt;- v2Parser
  let v3 = f v1 v2
  pure $ Result v1 v2 v3</code></pre><p>The way <code>do</code> blocks are de-sugared lead to the use of <code>&gt;&gt;=</code> when there are <code>let</code> bindings.</p><p>You'll run in a similar issue if you try to pattern-match on the left-hand side of <code>&lt;-</code>:</p><pre><code class="language-haskell">-- this requires a `Monad` instance on `EnvParser`
myParser :: EnvParser Result
myParser = do
  (v1, v2) &lt;- bothVsParser
  -- you can bypass the limitation with a lazy pattern
  -- ~(v1, v2) &lt;- bothVsParser
  v3       &lt;- v3Parser
  pure $ Result v1 v2 v3</code></pre><p>Thankfully there is a simple solution for both issues: using a <code>let … in</code> expression as the last part (after <code>pure</code>). This is always possible for applicative composition.</p><pre><code class="language-haskell">myParser :: EnvParser Result
myParser = do
  v1 &lt;- v1Parser
  v2 &lt;- v2Parser
  pure $ let v3 = f v1 v2
          in Result v1 v2 v3
          
myOtherParser :: EnvParser Result
myOtherParser = do
  both &lt;- bothVsParser
  v3   &lt;- v3Parser
  pure $ let (v1, v2) = both
         in Result v1 v2 v3</code></pre><h2 id="what-did-we-learn-then">What did we learn then?</h2><figure class="kg-card kg-image-card kg-card-hascaption"><img src="https://s3-eu-west-1.amazonaws.com/tech-blog-fretlink/2020/04/youtube-video-gif.gif" class="kg-image" alt loading="lazy"><figcaption>I guess we learned to not <code>do</code> it again</figcaption></figure><p>There are multiple layers to the <code>do</code> notation. At first you can see it as a way to make Haskell look more imperative. It's a good beginner-friendly crutch, but you soon grow past it: function composition is what Haskell is all about, anyway. That's where I was for a long time. But using Haskell in a professional setting led me to a more nuanced opinion: the language provides different tools, it's up to me to use them in order to get something that best expresses my intent. Of course nothing is really perfect and <code>do</code> notation has its issues, but it can be of great help when it comes to choosing how to structure my code.</p><p>Haskell is a mature language and, perhaps a bit counter-intuitively, not that opinionated: it provides developers with a lot of tools and alternative ways to do the same thing. Personally, I love it because I can craft code in a way that lets me outline what I want, <em>depending on context</em>. Of course a pragmatic compromise like this is not the best choice for everyone, some may prefer <a href="https://golang.org">a more opinionated language</a> that makes more decisions for you.</p>
                </div>
            </section>
