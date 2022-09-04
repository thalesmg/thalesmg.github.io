--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import           Control.Applicative (empty)
import           Control.Concurrent (threadDelay, forkIO)
import           Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import           Control.Monad ((>=>), forever)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Functor        ((<&>))
import           Data.Maybe          (fromJust, fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import           GHC.Stack (HasCallStack)
import           Hakyll
import qualified Hakyll.Alectryon as Alectryon
import           System.IO (Handle, hFlush)
import           System.Process
import           Text.Blaze.Html (preEscapedToHtml, (!))
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Pandoc (Block(CodeBlock, RawBlock), Pandoc)
import           Text.Pandoc.Walk (walkM)

--------------------------------------------------------------------------------
main :: HasCallStack => IO ()
main = do
  pygmento <- pygmentsServer
  hakyll $ do
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

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match postsGlob $ do
        route $ setExtension "html"
        compile $
          getResourceBody
            >>= readPandoc
            >>= Alectryon.tryTransform_
            >>= withItemBody (pygments pygmento)
            <&> writePandoc
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
    mkTagUrl tagItem = do
      mRoute <- getRoute . tagsMakeId tags . itemBody $ tagItem
      pure . toUrl . fromJust $ mRoute

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

-- https://github.com/blaenk/blaenk.github.io/blob/2a10c4806ac5011e93060abf11a1960fbaaa61e5/src/Site/Pygments.hs
type Chans = (Chan (Text, Text), Chan TL.Text)

pygmentsServer :: IO Chans
pygmentsServer = do
  (Just inp, Just out, _, _) <-
    createProcess (proc "python" ["pygmento.py"])
    { std_out = CreatePipe
    , std_in = CreatePipe
    }
  reqChan <- newChan
  repChan <- newChan
  forkIO . forever $ do
    (lang, code) <- readChan reqChan
    formatted <- pygmentize (inp, out) lang code
    writeChan repChan formatted
  return (reqChan, repChan)

pygments :: Chans -> Pandoc -> Compiler Pandoc
pygments chans = walkM (generateCodeBlock chans)

generateCodeBlock :: Chans -> Block -> Compiler Block
generateCodeBlock chans (CodeBlock (_, classes, keyvals) contents) = do
  let lang = fromMaybe (if null classes then "text" else head classes) $ lookup "lang" keyvals

  code <- if lang == "text"
            then return $ renderHtml $ H.toHtml contents
            else callPygmentize chans lang contents

  let colored = renderHtml $ H.pre ! A.class_ "sourceCode" $ H.code ! A.class_ (H.toValue $ "sourceCode " <> lang) $ do
                  preEscapedToHtml code
      caption = maybe "" (renderHtml . H.figcaption . H.span . preEscapedToHtml) $ lookup "text" keyvals
      composed = renderHtml $ H.div ! A.class_ "codeBlock" $ do
                   preEscapedToHtml $ caption <> colored

  return $ RawBlock "html" (TL.toStrict composed)
generateCodeBlock _ x = return x

callPygmentize :: Chans -> Text -> Text -> Compiler TL.Text
callPygmentize (reqChan, repChan) lang contents = unsafeCompiler $ do
  writeChan reqChan (lang, contents)
  readChan repChan

pygmentize :: (Handle, Handle) -> Text -> Text -> IO TL.Text
pygmentize (pin, pout) lang contents = do
  let len = T.pack . show . BS.length . TE.encodeUtf8 $ contents
      -- REQUEST:  LANG\nLENGTH\nCODE
      request = TE.encodeUtf8 $ T.intercalate "\n" [lang, len, contents]
  BS.hPut pin request
  hFlush pin
  -- RESPONSE: LENGTH\nRESPONSE
  responseLength <- read . T.unpack . TE.decodeUtf8 <$> BS.hGetLine pout
  TL.fromStrict . TE.decodeUtf8 <$> BS.hGet pout responseLength
