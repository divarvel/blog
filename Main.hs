{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Functor ((<$>))
import Data.List (isPrefixOf)
import Data.Monoid (mappend)
import Data.Text (pack, unpack, replace, empty)

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy Files
    match "files/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (tagsCtx tags)
            >>= (externalizeUrls $ feedRoot feedConfiguration)
            >>= saveSnapshot "content"
            >>= (unExternalizeUrls $ feedRoot feedConfiguration)
            >>= loadAndApplyTemplate "templates/disqus.html" (tagsCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (tagsCtx tags)
            >>= relativizeUrls

    -- Render posts list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/posts.html" allPostsCtx
                >>= loadAndApplyTemplate "templates/default.html" allPostsCtx
                >>= relativizeUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- take 10 <$> recentFirst posts
            itemTpl <- loadBody "templates/postitem.html"
            list <- applyTemplateList itemTpl postCtx sorted
            makeItem list
                >>= loadAndApplyTemplate "templates/index.html" (homeCtx tags list)
                >>= loadAndApplyTemplate "templates/default.html" (homeCtx tags list)
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged " ++ tag
        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"
                        (constField "title" title `mappend`
                            constField "body" list `mappend`
                            defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                        (constField "title" title `mappend`
                            defaultContext)
                >>= relativizeUrls


    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            sorted <- take 10 <$> recentFirst posts
            renderRss feedConfiguration feedCtx (take 10 sorted)

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            sorted <- take 10 <$> recentFirst posts
            renderAtom feedConfiguration feedCtx sorted

    -- Read templates
    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

allPostsCtx :: Context String
allPostsCtx =
    constField "title" "All posts" `mappend`
    postCtx

homeCtx :: Tags -> String -> Context String
homeCtx tags list =
    constField "posts" list `mappend`
    constField "title" "Index" `mappend`
    field "taglist" (\_ -> renderTagList tags) `mappend`
    defaultContext

feedCtx :: Context String
feedCtx =
    bodyField "description" `mappend`
    postCtx

tagsCtx :: Tags -> Context String
tagsCtx tags =
    tagsField "prettytags" tags `mappend`
    postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Clément Delafargue - RSS feed"
    , feedDescription = "Musings about FP and CS"
    , feedAuthorName  = "Clément Delafargue"
    , feedAuthorEmail = "clement+blog@delafargue.name"
    , feedRoot        = "http://blog.clement.delafargue.name"
    }

externalizeUrls :: String -> Item String -> Compiler (Item String)
externalizeUrls root item = return $ fmap (externalizeUrlsWith root) item

externalizeUrlsWith :: String -- ^ Path to the site root
                    -> String -- ^ HTML to externalize
                    -> String -- ^ Resulting HTML
externalizeUrlsWith root = withUrls ext
  where
    ext x = if isExternal x then x else root ++ x

-- TODO: clean me
unExternalizeUrls :: String -> Item String -> Compiler (Item String)
unExternalizeUrls root item = return $ fmap (unExternalizeUrlsWith root) item

unExternalizeUrlsWith :: String -- ^ Path to the site root
                      -> String -- ^ HTML to unExternalize
                      -> String -- ^ Resulting HTML
unExternalizeUrlsWith root = withUrls unExt
  where
    unExt x = if root `isPrefixOf` x then unpack $ replace (pack root) empty (pack x) else x

postList :: Tags
         -> Pattern
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList tags pattern preprocess' = do
    postItemTpl <- loadBody "templates/postitem.html"
    posts <- loadAll pattern
    processed <- preprocess' posts
    applyTemplateList postItemTpl (tagsCtx tags) processed
