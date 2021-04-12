--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative (empty)
import           Data.Maybe          (fromJust)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    let postsGlob = "posts/*"
    tags <- buildTags postsGlob (fromCapture "tags/*.html")

    tagsRules tags $ \tag pat -> do
      let title = "Posts tagged " <> tag
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let ctx = listField "posts" (postCtxWithTags tags) (pure posts)
               <> constField "title" title
               <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/post-list.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match postsGlob $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsGlob
            let archiveCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) <>
                    constField "title" "Archives" <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll postsGlob
            let indexCtx =
                    listField "posts" (postCtxWithTags tags) (return posts) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
     tagsField "tags" tags
  <> allTagsField "tagsList" tags
  <> postCtx

-- https://github.com/ysndr/blog/blob/5e10a521ca3faea94f0d9c536f079d4e5a5780db/generator/Fields.hs#L205
allTagsField :: String -> Tags -> Context String
allTagsField name tags = listFieldWith name tagCtx mkPostTags
  where
    tagCtx :: Context String
    tagCtx = field "name" (pure . itemBody)
          <> field "url" mkTagUrl

    mkTagUrl :: Item String -> Compiler String
    mkTagUrl tagItem = toUrl <$> (fmap fromJust . getRoute . tagsMakeId tags . itemBody $ tagItem)

    mkPostTags :: Item String -> Compiler [Item String]
    mkPostTags postItem = do
      tgs <- getTags (itemIdentifier postItem)
      if null tgs
        then empty
        else mapM makeItem tgs

postCtx :: Context String
postCtx =
     dateField "date" "%Y-%m-%d"
  <> defaultContext
