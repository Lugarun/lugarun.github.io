--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Definition
import           Control.Monad ((>=>))
import           Text.Pandoc.Options (HTMLMathMethod(..), writerHTMLMathMethod)
import           Text.Pandoc.Walk (walk, walkM)
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Network.URI.Encode as URI (encode)
import qualified Data.Text as Text

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "docs"
  }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
  { feedTitle = "Lukas's Blog"
  , feedDescription = "Lukas's part of the blogosphere"
  , feedAuthorName = "Lukas Schmidt"
  , feedAuthorEmail = ""
  , feedRoot = "https://lugarun.github.io"
  }

feedCtx :: Context String
feedCtx = postCtx <> bodyField "description"


tikzFilter :: Block -> Compiler Block
tikzFilter (CodeBlock (id, "tikzpicture":extraClasses, namevals) contents) =
  (imageBlock . ("data:image/svg+xml;utf8," ++) . URI.encode . filter (/= '\n') . itemBody <$>) $
    makeItem (Text.unpack contents)
     >>= loadAndApplyTemplate (fromFilePath "templates/tikz.tex") (bodyField "body")
     >>= withItemBody (return . pack
                       >=> unixFilterLBS "rubber-pipe" ["--pdf"]
                       >=> unixFilterLBS "pdftocairo" ["-svg", "-", "-"]
                       >=> return . unpack)
  where imageBlock fname = Para [Image (id, "tikzpicture":extraClasses, namevals) [] (Text.pack fname, "")]
tikzFilter x = return x

main :: IO ()
main = hakyllWith config  $ do
    match ("images/*" ) $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/about.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    let pandocOptions = defaultHakyllWriterOptions {
        writerHTMLMathMethod = MathJax ""
    }
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ (pandocCompilerWithTransformM defaultHakyllReaderOptions pandocOptions $ walkM tikzFilter)
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
